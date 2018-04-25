package calculator

import calculator.parser.Parser
import calculator.parser.Trees.ComputeResult.{DoubleResult, MemAccess, MemAssignment}

import scala.io.Source

class Calculator(source: Source) extends Parser(source: Source) {
  def execute(): Unit = {
    try {
      computeSource match {
        case (DoubleResult, _, result) => println("Result : " + result)
        case (MemAssignment, variable, value) => println("Memory updated variable \"" + variable + "\" with value: " + value)
        case (MemAccess, variable, value) => println("Variable \"" + variable + "\" = " + value)
        case _ => println("Well, this is strange")
      }
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }
}