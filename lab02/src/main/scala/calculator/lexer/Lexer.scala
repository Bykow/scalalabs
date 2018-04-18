package calculator.lexer

import scala.io.Source

class Lexer(source: Source) {

  import Tokens._

  val numeric: List[Char] = ('0' to '9').toList
  val alphabetic: List[Char] = ('a' to 'z').toList ++ ('A' to 'Z').toList
  val alphanumeric: List[Char] = numeric ++ alphabetic ++ List('_')

  var position: Int = 0
  var ch: Char = ' '
  var eof: Boolean = false

  /** Works like an iterator, and returns the next token from the input stream. */
  def nextToken: Token = {
    if (eof) {
      position = source.pos
      setToken(EOF)
    } else {
      if (position == 0) nextChar
      position = source.pos

      val numPattern = "[0-9]".r
      val keywordPattern = "[a-zA-Z]".r
      ch match {
        case ' ' => skipToken
        case '+' => setToken(PLUS)
        case '-' => setToken(MINUS)
        case '*' => setToken(TIMES)
        case '/' => setToken(DIVIDE)
        case '%' => setToken(MODULO)
        case '^' => setToken(POWER)
        case '(' => setToken(LPAREN)
        case ')' => setToken(RPAREN)
        case '=' => setToken(EQSIGN)
        case numPattern(_) => setToken(NUMBER(readMultiple('0' to '9' toList)))
        case keywordPattern(_) => setToken(keywordOrId(readMultiple('a' to 'Z' toList)))
        case _ => setToken(BAD)
      }
    }
  }

  /** Checks and set if the multiple Char found is a keyword or a variable */
  def keywordOrId(str: String): TokenInfo = {
    str.toLowerCase match {
      case "factorial" => FACTORIAL
      case "gcd" => GCD
      case "sqrt" => SQRT
      case name => ID(name)
    }
  }

  /** Moves the iterator to the next Char and set previous Token */
  def setToken(tkn: TokenInfo): Token = {
    nextChar;
    Token(tkn).setPos(position)
  }

  /** Moves the iterator to the next Char and skip the current token, useful for empty Char */
  def skipToken: Token = {
    nextChar;
    nextToken
  }

  /** Reads multiple Char at once, useful for detecting variables and keywords */
  def readMultiple(allowed: List[Char]): String = {
    var str = "" + ch
    nextChar
    while (allowed.contains(ch) && !eof) {
      str += ch
      nextChar
    }
    str
  }

  /** Moves the iterator to the next Char of the input source */
  def nextChar: Unit = if (source.hasNext) ch = source.next() else {
    ch = ' ';
    eof = true
  }

  def fatalError(msg: String): Nothing = {
    println("Fatal error", msg)
    sys.exit(1)
  }
}