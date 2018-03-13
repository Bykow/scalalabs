var memory: Map[String, Double] = Map()

def op(oprand: Char, a: Double, b: Double) = (oprand, a, b) match {
  case ('+',a,b) => a+b
  case ('-',a,b) => a-b
  case ('*',a,b) => a*b
  case ('/',a,0) => "Can't devide by 0"
  case ('/',a,b) => a/b
  case ('!',a,_) => fac(a)
}

def fac(a: Double): Double = a match {
  case 1 => a
  case _ => a * fac(a-1)
}


op('+',2,3)
op('-',4,5)
op('*',3,2)
op('/',9,3)
op('/',5,0)

op('!', 3, 908)

