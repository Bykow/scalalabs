def isPrime(n: Int): Boolean = {
  val sqrtN = Math.sqrt(n).floor.toInt

  def primeHelper(n: Int, upperBound: Int): Boolean = (n, upperBound) match {
    case (_, 1) => true
    case (a, b) if a % b == 0 => false
    case _ => primeHelper(n, upperBound - 1)
  }

  primeHelper(n, sqrtN)
}

case class Book (title: String, authors: List[String])

val books: List[Book] = List(
  Book(
  "Structure and Interpretation of Computer Programs",
  List("Abelson, Harald", "Sussman, Gerald J.")
  ),
  Book(
    "Introduction to Functional Programming",
    List("Bird, Richard", "Wadler, Phil")
  ),
  Book(
    "Effective Java",
    List("Bloch, Joshua")
  ),
  Book(
    "Java Puzzlers",
    List("Bloch, Joshua", "Gafter, Neal")
  ),
  Book(
    "Programming in Scala",
    List("Odersky, Martin", "Spoon, Lex", "Venners, Bill") )
)

(for {
  b1 <- books
  b2 <- books
  if (b1 != b2)
  a1 <- b1.authors
  b2.authors.contains(a1)
} yield a1).distinct


for (b<-books; a<-b.authors if a startsWith "Bird") yield b.title

books.flatMap(book => for (a<-book.authors if a startsWith "Bird") yield book.title))

books.flatMap(book => flatMap((book.authors).withFilter(author => startsWith "Bird")))