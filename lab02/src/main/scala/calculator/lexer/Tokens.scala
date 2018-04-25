package calculator.lexer

import calculator.Positional

object Tokens {

  sealed trait TokenClass {
    def tokenClass: this.type = this
  }

  sealed trait TokenInfo {
    def tokenClass: TokenClass
  }

  /** Token */
  class Token(val info: TokenInfo) extends Positional {
    override def toString: String = info.toString

    def tokenClass: TokenClass = info.tokenClass
  }

  case class NUMLIT(value: String) extends TokenInfo with TokenClass

  case class ID(name: String) extends TokenInfo with TokenClass

  /** Tokens */
  case object BAD extends TokenInfo with TokenClass // represents incorrect tokens.

  case object EOF extends TokenInfo with TokenClass // represents end of file

  /** Operations */
  case object PLUS extends TokenInfo with TokenClass // +

  case object MINUS extends TokenInfo with TokenClass // -

  case object MULT extends TokenInfo with TokenClass // *

  case object DIVIDE extends TokenInfo with TokenClass // /

  case object MODULO extends TokenInfo with TokenClass // %

  case object POWER extends TokenInfo with TokenClass // ^

  case object FACTORIAL extends TokenInfo with TokenClass // !

  case object COMMA extends TokenInfo with TokenClass // ,

  case object NEG extends TokenInfo with TokenClass // (-x)

  /** Functions */
  case object GCD extends TokenInfo with TokenClass // gcd

  case object SQRT extends TokenInfo with TokenClass // sqrt

  /** Special tokens */
  case object EQSIGN extends TokenInfo with TokenClass // =

  case object LPAREN extends TokenInfo with TokenClass // (

  case object RPAREN extends TokenInfo with TokenClass // )

  object Token {
    def apply(info: TokenInfo): Token = new Token(info)

    def unapply(token: Token): Option[TokenInfo] = Some(token.info)
  }

}