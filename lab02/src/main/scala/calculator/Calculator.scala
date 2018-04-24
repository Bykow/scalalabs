package calculator

import calculator.parser.Parser

import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {
  def execute(): Unit = {
    try {
      computeSource match {
        case (0, _, result) => println("Result : " + result)
        case (1, variable, value) => println("Memory updated variable \"" + variable + "\" with value: " + value)
        case (2, variable, value) => println("Variable \"" + variable + "\" = " + value)
        case (5, msg, _) => println("Error: " + msg)
      }
    } catch {
      case e : Exception => println(e.getMessage)
    }
//    printTree
  }
}