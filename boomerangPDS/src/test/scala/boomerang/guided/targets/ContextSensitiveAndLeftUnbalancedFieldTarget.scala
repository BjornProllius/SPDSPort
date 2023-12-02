package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalancedFieldTarget {

  def main(args: Array[String]): Unit = {
    val myObject = new MyObject
    context(myObject.field)
  }

  private def context(barParam: String): Unit = {
    val bar = doPassArgument(barParam)
    new File(bar)
  }

  private def doPassArgument(param: String): String = {
    new String(param)
  }

  private class MyObject {
    val field: String = "bar"
  }
}