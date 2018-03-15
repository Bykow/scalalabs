import scala.annotation.tailrec

var memory: Map[String, Double] = Map()
val epsilon = 0.0001

def opBinary(op: Char, a: Double, b: Double) = (op, a, b) match {
  case ('+', x, y) => x + y
  case ('-', x, y) => x - y
  case ('*', x, y) => x * y
  case ('/', _, 0) => "Can't divide by 0"
  case ('/', x, y) => x / y
  case (_, _, _) => "Invalid operation"
}

def opMemory(str: String, value: Double): Unit = {
  memory += (str -> value)
}

def factorial(a: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = a match {
    case 1 => acc
    case _ => loop(a - 1, acc * a)
  }

  loop(a, 1)
}

def gcd(a: Int, b: Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}

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

sqrt(9)

prime(11)

egcd(5, 167)

modInvert(3, 12)
