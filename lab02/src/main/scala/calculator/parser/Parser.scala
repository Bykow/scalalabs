package calculator.parser

import calculator.Main.memory
import calculator.lexer._

import scala.io.Source

class Parser(source: Source) extends Lexer(source: Source) {

  import Trees._
  import calculator.lexer.Tokens._

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  def computeSource: (Int, String, Double) = {
    readToken
    parseExpr.compute
  }

  def printTree: Unit = {
    readToken
    println(parseExpr)
  }

  /** ""Eats"" the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit = if (tokenClass == currentToken.info.tokenClass) readToken else expected(tokenClass)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken)

  private def parseExpr: ExprTree = {
    parseEquals
  }

  private def parseEquals: ExprTree = {

    val e = parsePlusMinus
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

  private def parsePlusMinus: ExprTree = {
    var e = parseMultDivide
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

  private def parseMultDivide: ExprTree = {
    var e = parseModulo
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

  private def parseModulo: ExprTree = {
    var e = parsePower
    while (currentToken.info == MODULO) {
      eat(MODULO)
      e = Modulo(e, parsePower)
    }
    e
  }

  private def parsePower: ExprTree = {
    var e = parseFactorial
    while (currentToken.info == POWER) {
      eat(POWER)
      e = Power(e, parseFactorial)
    }
    e
  }

  private def parseFactorial: ExprTree = {
    var e = parseSimpleExpr
    while (currentToken.info == FACTORIAL) {
      eat(FACTORIAL)
      e = Factorial(e)
    }
    e
  }

  private def parseSimpleExpr: ExprTree = {
    // Here you want to match simple expressions such as NUM(value) and parse them (for example with the parseExprTreeToken method).
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

  private def parseParenthesisTuple: ExprTree = {
    eat(GCD)
    eat(LPAREN)
    val lhs = parsePlusMinus
    eat(COMMA)
    val rhs = parsePlusMinus
    eat(RPAREN)
    Gcd(lhs, rhs)
  }

  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    readToken
    retTree
  }

  /** update currentToken using nextToken in the Lexer. */
  def readToken: Unit = {
    currentToken = nextToken
  }

  private def parseParenthesis: ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }
}

