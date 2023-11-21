package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalancedFieldTarget {
  def main(args: Nothing*): Unit = {
    val myObject = new ContextSensitiveAndLeftUnbalancedFieldTarget.MyObject
    context(myObject.field)
  }

  private def context(barParam: Nothing): Unit = {
    val bar = doPassArgument(barParam)
    new Nothing(bar)
  }

  private def doPassArgument(param: Nothing) = new Nothing(param)

  private class MyObject {
    private val field = "bar"
  }
}