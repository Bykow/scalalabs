package calculator.parser

import calculator.Main.memory

import scala.annotation.tailrec

object Trees {

  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: (Int, String, Double) = this match {
      case NumLit(value)            => (0, "", value toDouble)
      case Plus(lhs, rhs)           => (0, "", lhs.compute._3 + rhs.compute._3)
      case Minus(lhs, rhs)          => (0, "", lhs.compute._3 - rhs.compute._3)
      case Mult(lhs, rhs)           => (0, "", lhs.compute._3 * rhs.compute._3)
      case Divide(lhs, rhs)         => (0, "", lhs.compute._3 / rhs.compute._3)
      case Power(lhs, rhs)          => (0, "", power(lhs.compute._3, rhs.compute._3))
      case Modulo(lhs, rhs)         => (0, "", lhs.compute._3 % rhs.compute._3)
      case Factorial(value)         => (0, "", factorial(value.compute._3))
      case Gcd(lhs, rhs)            => (0, "", gcd(lhs.compute._3, rhs.compute._3))
      case Sqrt(value)              => (0, "", sqrt(value.compute._3))
      case Assign(variable, value)  => (1, variable.value,  value.compute._3)
      case Identifier(value)        => (2, value, memory.getOrElse(value, Double.NegativeInfinity))
      case Neg(value)               => (0, "", -value.compute._3)
      case _                        => (5, "",  0)
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

  case class Gcd(lhs: ExprTree, rhs: ExprTree) extends ExprTree

  case class Sqrt(operand: ExprTree) extends ExprTree

  case class Neg(value: ExprTree) extends ExprTree

  /** Leaves Expression Trees */
  case class NumLit(value: String) extends ExprTree {
    override def toString: String = "Literal('" + value + "')"
  }

  case class Identifier(value: String) extends ExprTree {
    override def toString: String = "Identifier('" + value + "')"
  }

  /**
    * Power function
    *
    * @param base
    * @param exp
    * @return result
    */
  def power(base: Double, exp: Double): Double = {
    @tailrec
    def powerHelper(result: Double, exp: Double): Double = exp match {
      case 0 => result
      case _ => powerHelper(result*base, exp-1)
    }
    powerHelper(1, exp)
  }

  /**
    * Computes the factorial of given Int.
    * Uses tailrec optimization
    *
    * @param a int to factorial
    * @return Int result
    */
  def factorial(a: Double): Double = {
    @tailrec
    def loop(a: Double, acc: Double): Double = a match {
      case 1 => acc
      case _ => loop(a - 1, acc * a)
    }

    loop(a, 1)
  }

  /**
    * Computes the gcd between two Ints
    * Uses tailrec optimization
    *
    * @param a
    * @param b
    * @return Int result
    */
  def gcd(a: Double, b: Double): Double = (a,b) match {
    case (_,0) => a
    case _ => gcd(b, a % b)
  }


  val epsilon = 0.00001

  /**
    * Computes the square root of given Double
    * Uses tailrec optimization
    *
    * @param n operand
    * @return Double, result
    */
  def sqrt(n: Double): Double = {
    if (n == 0) return 0
    @tailrec
    def approx(n: Double, x: Double): Double = {
      math.abs(x * x - n) / n match {
        case e if e < epsilon => x
        case e if e >= epsilon => approx(n, (x + (n / x)) / 2)
      }
    }

    approx(n, 1.0)
  }

}