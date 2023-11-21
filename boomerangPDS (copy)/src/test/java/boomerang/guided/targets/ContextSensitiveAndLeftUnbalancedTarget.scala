package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalancedTarget {
  def main(args: Nothing*): Unit = {
    context("bar")
  }

  private def context(barParam: Nothing): Unit = {
    val bar = doPassArgument(barParam)
    val foo = doPassArgument("foo")
    val quz = doPassArgument("quz")
    new Nothing(bar)
    new Nothing(foo)
    new Nothing(quz)
  }

  private def doPassArgument(param: Nothing) = new Nothing(param)
}