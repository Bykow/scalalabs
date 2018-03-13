object ScalaApp {
  var prenom: String = ""
  var nom: String = ""
  val dateDeNaissance = "19.12.1992"

  prenom = "Lawrence"
  nom = "Stalder"

  def foo(): Unit = {
    println("Bonjour, je m’appelle " + prenom + " " + nom + " et je suis né le " + dateDeNaissance + ".")
  }

  println("Boucle I")
  var cnt = 0;
  do {
    foo()
    if (cnt < 0) {
      cnt -= 1
    }
  } while (cnt != 0)


  println("Boucle II")
  while (cnt < 5) {
    foo()
    cnt += 1
  }

  println("Boucle III")
  for (cnt <- 0 until 5) {
    foo()
  }

  println("Boucle IV")
  for (cnt <- 0 until 5) {
    cnt match {
      case 0 => println(nom)
      case _ => println(prenom)
    }
  }


  cnt = 0
  while (cnt != 100) {
    if (cnt%2 != 0 && (cnt%3 == 0 || cnt%5 == 0)) {
      print(cnt + " ")
    }
    cnt += 1
  }

  for (cnt <- 0 until 100 if cnt % 2 != 0 && (cnt % 3 == 0 || cnt % 5 == 0)) print(cnt + " ")


  def max(a: Int, b : Int) = if (a<b) b else a
  max(2, 4)

  def brainfuck(str : String) = str match {
    case "World" => "Hello"
    case "Hello" => "World"
    case _ => "Goodbye"
  }
  brainfuck("Wor")


  for(i <- 0 to 2; j <- 0 to 3; k <- 0 to 3) println("Hello World !")


  def func1(x : Int) : Int = x match {
    case a if a  > 100 => a
    case b if b % 7 == 0 => func1(b + 8)
    case c if c % 2 == 1 => func1(c + 12)
    case _ => func1(x+1)
  }
  val x = 0
  func1(x)


  def func2(x: Int, y: Int, z: Int) : Int = z match {
    case 0 => func2(x,y,2) + func2(x,y,1)
    case a if a % 2 == 0 => (x*x)+(y*y)
    case b if b % 2 == 1 => (x+y)*(x+y)
  }

  func2(1,2,0)

  def func3(x: Int, y: Int) : Int = x match {
    case a == y => a
    case b < y => y
    case c > y => def func4(x: Int, y: Int) : Int = (x,y) match {
      case (a,b) if a % 2 == 1 => 2*a + 3*y
      case (c,d) if d % 2 == 1 => 4*c - 7*d
      case (e,f) if e % 3 == 0 && f % 4 == 0 => (e*e) + (f*f*f)
    }
    case d < y && d > 4  => 2*d
    case
  }

}