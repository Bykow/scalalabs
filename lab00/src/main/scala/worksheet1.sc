import scala.annotation.tailrec

// #### EX 1 ####
val list = List(1,2,3,4)
val vec = Vector("Hello", "Mother", "Fucker", "You", "Dun")

def sum1(a: List[Int]): Int = a match {
  case Nil => 0
  case x :: tail => x + sum1(tail)
}

def sum2(ints: List[Int]): Int = {
  @tailrec
  def sumAccumulator(ints: List[Int], accum: Int): Int = ints match {
    case Nil => accum
    case x :: tail => sumAccumulator(tail, accum + x)
  }
  sumAccumulator(ints, 0)
}

def sum3(xs: List[Int]): Int = {
  if (xs.isEmpty) 0
  else xs.head + sum3(xs.tail)
}

def sumWithReduce(ints: List[Int]) = {
  ints.reduceLeft(_ + _)
}


// #### EX 2 ####
def mapping(vec: Vector[String], list: List[Int]): Map[Int, String] = {
  (list zip vec).toMap
}

println(mapping(vec, list))


// #### EX 3 ####
val vec2 = (1 to 100).toVector
def custom(x: Int): Int = x match {
  case a if a%3 == 0 => a*a*a
  case b if b%2 == 0 => b*b
  case _ => -1
}

println(vec2.map(x => custom(x)))


// #### EX 4 ####
val vec3 = (1 to 10).toVector
def factoriel(x: Int): Int = x match {
  case 1 => 1
  case a => a * factoriel(a - 1)
}

def fibonacci(x: Int): Int = x match {
  case 0 | 1 => x
  case _ => fibonacci(x-1) + fibonacci(x-2)
}

vec3.map(a => if (a%2 == 0) factoriel(a) else fibonacci(a))


// #### EX 5 ####
val vec4 = (1 to 5).toVector
val list2 = (1 to 10).toList
def fuckmeup(vec: Vector[Int], list: List[Int]): Vector[List[Int]] = {
  vec4.map(x => list2.map(_ * x))
}

println(fuckmeup(vec4, list2))


// #### EX 6 ####
val list3 = List(1, 5, 7, 2, 8, 4, 3, 6, 9)
def customSort(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case x :: xs => insert(x, customSort(xs))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y :: ys =>
    if(y >= x) x :: xs
    else y :: insert(x, ys)
}

println(customSort(list3))

// #### EX 7 ####
def filt(l: List[Int], p: Int => Boolean) : List[Int] = l match {
  case Nil => Nil
  case x :: xs if p(x) => x :: filt(xs, p)
  case x :: xs => filt(xs, p)
}


println(filt(list3, _%2 == 0))