package calculator.parser

object Trees {

  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: Double = this match {
      case NumLit(value) => value toDouble
      case Plus(lhs, rhs) => lhs.compute + rhs.compute
      case Minus(lhs, rhs) => lhs.compute - rhs.compute
      case Mult(lhs, rhs) => lhs.compute * rhs.compute
      case Divide(lhs, rhs) => lhs.compute / rhs.compute
      case Power(lhs, rhs) => Math.pow(lhs.compute, rhs.compute)
      case _ => 0
    }
  }

  /** Nodes Expression Trees */
  /** lhs: left hand side, rhs: right hand side */
  case class Assign(ident: Identifier, value: ExprTree) extends ExprTree

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Mult(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Divide(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Modulo(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Power(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Factorial(operand: ExprTree) extends ExprTree

  case class Gcd(operand: ExprTree) extends ExprTree

  case class Sqrt(operand: ExprTree) extends ExprTree

  /** Leaves Expression Trees */
  case class NumLit(value: String) extends ExprTree {
    override def toString: String = "Literal('" + value + "')"
  }

  case class Identifier(value: String) extends ExprTree {
    override def toString: String = "Identifier('" + value + "')"
  }

}