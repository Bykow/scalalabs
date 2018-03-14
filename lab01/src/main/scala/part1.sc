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

def opUnary(op: String, a: Double) = (op, a) match {
  case ("!", x) => factorial(x)
  case ("sqrt", x) => sqrt(x)
}

def opMemory(str: String, value: Double): Unit = {
  memory += (str -> value)
}

def factorial(a: Double): Double = {
  def loop (a: Double, acc: Double): Double = a match {
    case 1 => acc
    case _ => loop(a - 1, acc * a)
  }
  loop(a, 1.0)
}

def gcd(a: Double, b: Double): Double = {
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
      val real1 = (- b + sqrtDelta) / (2 * a)
      val real2 = (- b - sqrtDelta) / (2 * a)
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
  def approx(n: Double, x: Double): Double = {
      math.abs(x * x - n) / n match {
      case e if e < epsilon => x
      case e if e >= epsilon => approx(n, (x + (n / x)) / 2)
    }
  }
  approx(n, 1.0)
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

solve(1,6,9)

sqrt(9)

