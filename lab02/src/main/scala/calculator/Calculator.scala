package calculator

import calculator.parser.Parser

import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {
  def execute(): Unit = {
//    printTree
    computeSource match {
      case Double.NegativeInfinity => println("Memory updated !")
      case result => println("Result : " + result)
    }
  }
}