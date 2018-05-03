val rand = scala.util.Random

def slice(i: Int, k: Int, ls: List[Any]) : List[Any] = (i,k,ls) match {
  case (a, b, _) if a < 0 || b < 1 => throw new Exception
  case (_, _, Nil) => throw new Exception
  case (0, 1, h::tail) => List(h)
  case (0, k, h::tail) => List(h) ::: slice(0, k-1, tail)
  case (i, k, h::tail) => slice(i-1, k-1, tail)
}
slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))


def rotate(n: Int, ls: List[Any]) : List[Any] = (n, ls) match {
  case (0, xs) => xs
  case (m, xs) if m < 0 => rotate(m + xs.size, xs)
  case (m, h::tail) if m > 0 => rotate(m-1, tail) ::: List(h)
}
rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))


def removeAt(n: Int, ls: List[Any]) : (List[Any], Any) = ls.splitAt(n) match {
  case (Nil, _) if n < 0 => throw new NoSuchElementException
  case (pre, e :: post)  => (pre ::: post, e)
  case (pre, Nil)        => throw new NoSuchElementException
}
removeAt(1, List('a, 'b, 'c, 'd))


def insertAt(m: Int, n: Int, ls: List[Int]) : List[Int] = ls.splitAt(n) match {
  case (pre, post) => pre ::: List(m) ::: post
}
insertAt(3, 2, List(1, 2, 4, 5))


def randomSelect(n: Int, ls: List[Any]) : List[Any] = (n, ls) match {
  case (0, _) => Nil
  case (a, xs) => randomSelect(n-1, removeAt(rand.nextInt(xs.size), xs)._1)
}
randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
