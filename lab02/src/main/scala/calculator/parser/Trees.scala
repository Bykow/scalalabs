package calculator.parser

import calculator.Main.memory
import calculator.parser.Trees.ComputeResult.{ComputeResult, DoubleResult, MemAccess, MemAssignment}

import scala.annotation.tailrec

object Trees {

  val epsilon = 0.00001

  /** Nodes Expression Trees */

  /**
    * Power function
    *
    * @param base the base
    * @param exp  the exponent
    * @return result the result
    */
  def power(base: Double, exp: Double): Double = {
    @tailrec
    def powerHelper(result: Double, exp: Double): Double = exp match {
      case 0 => result
      case _ => powerHelper(result * base, exp - 1)
    }

    powerHelper(1, exp)
  }

  /**
    * Computes the factorial of given Int.
    * Uses tailrec optimization
    *
    * @param a int to factorial
    * @return result the result
    */
  def factorial(a: Double): Double = {
    @tailrec
    def loop(a: Double, acc: Double): Double = a match {
      case 0 => 1
      case 1 => acc
      case _ => loop(a - 1, acc * a)
    }

    loop(a, 1)
  }

  /**
    * Computes the gcd between two Ints
    * Uses tailrec optimization
    *
    * @param a the first number
    * @param b the second number
    * @return result the result
    */
  def gcd(a: Double, b: Double): Double = (a, b) match {
    case (_, 0) => a
    case _ => gcd(b, a % b)
  }

  /**
    * Computes the square root of given Double
    * Uses tailrec optimization
    *
    * @param n the operand
    * @return result the result
    */
  def sqrt(n: Double): Double = {
    if (n <= 0) return 0

    @tailrec
    def approx(n: Double, x: Double): Double = {
      math.abs(x * x - n) / n match {
        case e if e < epsilon => x
        case e if e >= epsilon => approx(n, (x + (n / x)) / 2)
      }
    }

    approx(n, 1.0)
  }

  /**
    * 0: normal result
    * 1: assignment in memory
    * 2: access to memory
    *
    * Ideally, refactor with enum
    */
  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: (ComputeResult, String, Double) = this match {
      case NumLit(value) => (DoubleResult, "", value toDouble)
      case Plus(lhs, rhs) => (DoubleResult, "", lhs.compute._3 + rhs.compute._3)
      case Minus(lhs, rhs) => (DoubleResult, "", lhs.compute._3 - rhs.compute._3)
      case Mult(lhs, rhs) => (DoubleResult, "", lhs.compute._3 * rhs.compute._3)
      case Divide(lhs, rhs) => {
        val temp = rhs.compute._3
        if (temp != 0) (DoubleResult, "", lhs.compute._3 / temp) else throw new Exception("Can't divide by 0")
      }
      case Power(lhs, rhs) => (DoubleResult, "", power(lhs.compute._3, rhs.compute._3))
      case Modulo(lhs, rhs) => {
        val temp = rhs.compute._3
        if (temp != 0) (DoubleResult, "", lhs.compute._3 % temp) else throw new Exception("Can't modulo by 0")
      }
      case Factorial(value) => (DoubleResult, "", factorial(value.compute._3))
      case Gcd(lhs, rhs) => (DoubleResult, "", gcd(lhs.compute._3, rhs.compute._3))
      case Sqrt(value) => {
        val temp = value.compute._3
        if (temp >= 0) (DoubleResult, "", sqrt(value.compute._3)) else throw new Exception("Sqrt for negative number is undefined")
      }
      case Assign(variable, value) => (MemAssignment, variable.value, value.compute._3)
      case Identifier(value) => (MemAccess, value, memory.getOrElse(value, throw new Exception("Unknown variable \"" + value + "\"")))
      case Neg(value) => (DoubleResult, "", -value.compute._3)
      case _ => throw new Exception("Error, no Token found (_)")
    }
  }

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

  object ComputeResult extends Enumeration {
    type ComputeResult = Value
    val DoubleResult, MemAssignment, MemAccess = Value

  }

}