package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalancedThisFieldTarget {
  def main(args: Nothing*): Unit = {
    new ContextSensitiveAndLeftUnbalancedThisFieldTarget.MyObject().context()
  }

  object MyObject {
    private def doPassArgument(param: Nothing) = new Nothing(param)
  }

  class MyObject {
    private val field = "bar"

    def context(): Unit = {
      val bar = MyObject.doPassArgument(field)
      new Nothing(bar)
    }
  }
}