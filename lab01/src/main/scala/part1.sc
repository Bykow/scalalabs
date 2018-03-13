var memory: Map[String, Double] = Map()

def op(op: Char, a: Double, b: Double) = (op, a, b) match {
  case ('+', x, y) => x + y
  case ('-', x, y) => x - y
  case ('*', x, y) => x * y
  case ('/', _, 0) => "Can't divide by 0"
  case ('/', x, y) => x / y
  case ('!', x, _) => factorial(x)
  case (_, _, _) => "Invalid operation"
}

def factorial(a: Double): Double = a match {
  case 1 => 1
  case n => n * factorial(n - 1)
}

op('+', 2, 3)
op('-', 4, 5)
op('*', 3, 2)
op('/', 9, 3)
op('/', 5, 0)

op('!', 3, 908)

