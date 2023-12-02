package boomerang.guided.targets

import java.io.File

object ContextSensitiveTarget {

  def main(args: Array[String]): Unit = {
    val bar = doPassArgument("bar")
    val foo = doPassArgument("foo")
    val quz = doPassArgument("quz")
    new File(bar)
    new File(foo)
    new File(quz)
  }

  private def doPassArgument(param: String): String = {
    new String(param)
  }
}
