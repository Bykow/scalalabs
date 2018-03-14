var memory: Map[String, Double] = Map()

def opBinary(op: Char, a: Double, b: Double) = (op, a, b) match {
  case ('+', x, y) => x + y
  case ('-', x, y) => x - y
  case ('*', x, y) => x * y
  case ('/', _, 0) => "Can't divide by 0"
  case ('/', x, y) => x / y
  case (_, _, _) => "Invalid operation"
}

//def opUnary(op: Char, a: Double) = (op, a) match {
//  case ('!', x) => factorial(x)
//}

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

def solve(a: Double, b: Double, c: Double): String = {
  val x = (b * b) - (4 * a * c)
    x match {
      case x if x > 0 => "hi"
    }
}


opBinary('+', 2, 3)
opBinary('-', 4, 5)
opBinary('*', 3, 2)
opBinary('/', 9, 3)
opBinary('/', 5, 0)

//opUnary('!', 69)

factorial(3)

opMemory("a", 3)

memory.get("a")

gcd(167, 5)

