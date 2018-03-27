import scala.annotation.tailrec

var memory: Map[String, Double] = Map()
val epsilon = 0.0001

/**
  * Computes an binary operation given two operands and one operator
  *
  * @param op the operator
  * @param a operand a
  * @param b operand b
  * @return Any, String if an error has occured, double otherwise
  */
def opBinary(op: Char, a: Double, b: Double) = (op, a, b) match {
  case ('+', x, y) => x + y
  case ('-', x, y) => x - y
  case ('*', x, y) => x * y
  case ('^', x, y) => power(x.toInt,y.toInt)
  case ('/', _, 0) => "Can't divide by 0"
  case ('%', _, 0) => "Can't mod by 0"
  case ('%', x, y) => x % y
  case ('/', x, y) => x / y
  case _ => "Invalid operation"
}

/**
  * Memory management, adds a given value to a string (variable name)
  *
  * @param str name of the variable
  * @param value value in double
  */
def opMemory(str: String, value: Double): Unit = {
  memory += (str -> value)
}

/**
  * Power function
  *
  * @param base
  * @param exp
  * @return result
  */
def power(base: Int, exp: Int): Double = {
  @tailrec
  def _power(result: Int, exp: Int): Int = exp match {
    case 0 => result
    case _ => _power(result*base, exp-1)
  }
  _power(1, exp).toDouble
}

/**
  * Computes the factorial of given Int.
  * Uses tailrec optimization
  *
  * @param a int to factorial
  * @return Int result
  */
def factorial(a: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = a match {
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
def gcd(a: Int, b: Int): Int = (a,b) match {
  case (_,0) => a
  case _ => gcd(b, a % b)
}

/**
  * Solves an equation of second degree
  *
  * @param a x^2 coeficient
  * @param b x^1 coeficient
  * @param c x^0 coeficient
  * @return (Any, Any) depends on real or complex solutions
  */
def solve(a: Double, b: Double, c: Double): (Any, Any) = {
  def delta(a: Double, b: Double, c: Double): Double = (b * b) - (4 * a * c)

  delta(a, b, c) match {
    case 0 =>
      val double = (-b) / (2 * a)
      (double, double)
    case delta if delta > 0 =>
      val sqrtDelta = math.sqrt(delta)
      val real1 = (-b + sqrtDelta) / (2 * a)
      val real2 = (-b - sqrtDelta) / (2 * a)
      (real1, real2)
    case delta if delta < 0 =>
      val sqrtDelta = math.sqrt(-delta)
      val realPart = (-b) / (2 * a)
      val imaginaryPart1 = sqrtDelta / (2 * a)
      val imaginaryPart2 = (-sqrtDelta) / (2 * a)
      ((realPart, imaginaryPart1), (realPart, imaginaryPart2))
  }
}

/**
  * Computes the square root of given Double
  * Uses tailrec optimization
  *
  * @param n operand
  * @return Double, result
  */
def sqrt(n: Double): Double = {
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
  * Return whether n is prime or not
  * Uses tailrec optimization
  *
  * @param n
  * @return String
  */
def prime(n: Int): String = {
  val sqrtN = sqrt(n).floor.toInt

  @tailrec
  def primeHelper(n: Int, upperBound: Int): String = (n, upperBound) match {
    case (_, 1) => n + " is prime !"
    case (a, b) if a % b == 0 => "Not a prime number"
    case _ => primeHelper(n, upperBound - 1)
  }

  primeHelper(n, sqrtN)
}

/**
  * Euclidian extended gcd
  *
  * @param a
  * @param b
  * @return Euclidian decomposition
  */
def egcd(a: Int, b: Int): (Int, Int, Int) = {

  @tailrec
  def egcdHelper(r1: Int, u1: Int, v1: Int, r2: Int, u2: Int, v2: Int): (Int, Int, Int) = {
    r2 match {
      case x if x != 0 =>
        val q = r1 / r2
        egcdHelper(r2, u2, v2, r1 - q * r2, u1 - q * u2, v1 - q * v2)
      case _ => (u1, v1, r1)
    }
  }

  egcdHelper(a, 1, 0, b, 0, 1)
}

/**
  * Invert modulo
  *
  * @param a
  * @param b
  * @return Any, String if mod invert does not exists, Int otherwise
  */
def modInvert(a: Int, b: Int): Any = {
  egcd(a, b) match {
    case (_, _, z) if z != 1 => "Mod Invert does not exists"
    case (x, _, _) => x
  }
}


opBinary('+', 2, 3)
opBinary('-', 4, 5)
opBinary('*', 3, 2)
opBinary('/', 9, 3)
opBinary('/', 5, 0)

factorial(3)

opMemory("a", 3)

memory.get("a")

gcd(167, 5)

solve(1, 6, 9)

math.sqrt(-9)

prime(11)

egcd(5, 167)

modInvert(3, 12)

power(2, 2)
