package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalancedTarget {

  def main(args: Array[String]): Unit = {
    context("bar")
  }

  private def context(barParam: String): Unit = {
    val bar = doPassArgument(barParam)
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