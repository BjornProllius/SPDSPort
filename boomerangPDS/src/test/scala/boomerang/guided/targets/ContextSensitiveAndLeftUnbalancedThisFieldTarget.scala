package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalancedThisFieldTarget {

  def main(args: Array[String]): Unit = {
    new MyObject().context()
  }

  class MyObject {
    private val field: String = "bar"

    private def doPassArgument(param: String): String = {
      new String(param)
    }

    def context(): Unit = {
      val bar = doPassArgument(field)
      new File(bar)
    }
  }
}