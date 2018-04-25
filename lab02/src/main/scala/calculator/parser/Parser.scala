package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import calculator.parser.Trees.ComputeResult.ComputeResult

import scala.io.Source

class Parser(source: Source) extends Lexer(source: Source) {

  import Trees._
  import calculator.lexer.Tokens._

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  /** Compute the result of the tree returned by the Lexer */
  def computeSource: (ComputeResult, String, Double) = {
    readToken
    parseExpr.compute
  }

  /** Print the tree returned by the Lexer, use for debugging */
  def printTree: Unit = {
    readToken
    println(parseExpr)
  }

  /** Update currentToken using nextToken in the Lexer. */
  def readToken: Unit = {
    currentToken = nextToken
  }

  /** ""Eats"" the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit = if (tokenClass == currentToken.info.tokenClass) readToken else expected(tokenClass)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken)

  /** Start parsing an expression */
  private def parseExpr: ExprTree = {
    parseEquals
  }

  /** Parse an assignation */
  private def parseEquals: ExprTree = {
    val e = parsePlusMinus // Priority
    if (currentToken.info == EQSIGN) {
      eat(EQSIGN)
      e match {
        case id@Identifier(_) => {
          val rhs = parseEquals
          rhs match {
            case Assign(_, _) => fatalError("Invalid variable declaration !")
            case _ => {
              memory += (id.value -> rhs.compute._3)
              Assign(id, rhs)
            }
          }
        }
        case _ => fatalError("Invalid variable declaration !")
      }
    } else {
      e
    }
  }

  /** Parse addition and substraction */
  private def parsePlusMinus: ExprTree = {
    var e = parseMultDivide // Priority
    while (currentToken.info == PLUS || currentToken.info == MINUS) {
      if (currentToken.info == PLUS) {
        eat(PLUS)
        e = Plus(e, parseMultDivide)
      } else {
        eat(MINUS)
        e = Minus(e, parseMultDivide)
      }
    }
    e
  }

  /** Parse multiplication and division */
  private def parseMultDivide: ExprTree = {
    var e = parseModulo // Priority
    while (currentToken.info == MULT || currentToken.info == DIVIDE) {
      if (currentToken.info == MULT) {
        eat(MULT)
        e = Mult(e, parseModulo)
      } else {
        eat(DIVIDE)
        e = Divide(e, parseModulo)
      }
    }
    e
  }

  /** Parse modulo */
  private def parseModulo: ExprTree = {
    var e = parsePower // Priority
    while (currentToken.info == MODULO) {
      eat(MODULO)
      e = Modulo(e, parsePower)
    }
    e
  }

  /** Parse power raising */
  private def parsePower: ExprTree = {
    var e = parseFactorial
    while (currentToken.info == POWER) {
      eat(POWER)
      e = Power(e, parseFactorial)
    }
    e
  }

  /** Parse factorial */
  private def parseFactorial: ExprTree = {
    var e = parseSimpleExpr
    while (currentToken.info == FACTORIAL) {
      eat(FACTORIAL)
      e = Factorial(e)
    }
    e
  }

  /** Parse either a literal, an id for assignation, parentheses, result negation, or functions like sqrt and gcd */
  private def parseSimpleExpr: ExprTree = {
    currentToken.info match {
      case LPAREN => parseParenthesis // Parenthesis
      case NUMLIT(value) => parseExprTreeToken(NumLit(value))
      case ID(value) => parseExprTreeToken(Identifier(value))
      case GCD => parseParenthesisTuple
      case SQRT => {
        eat(SQRT)
        Sqrt(parseParenthesis)
      }
      case NEG => {
        eat(NEG)
        Neg(parsePower)
      }
      case _ => expected(currentToken.tokenClass)
    }
  }

  /** Parse a tuple for arguments to functions */
  private def parseParenthesisTuple: ExprTree = {
    eat(GCD)
    eat(LPAREN)
    val lhs = parsePlusMinus // Go through priority again
    eat(COMMA)
    val rhs = parsePlusMinus
    eat(RPAREN)
    Gcd(lhs, rhs)
  }

  /** Parse a tree and return the given token (consume the tree) */
  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    readToken
    retTree
  }

  /** Parse parentheses for priority of operations */
  private def parseParenthesis: ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus // Go through priority again
    eat(RPAREN)
    ret
  }
}

