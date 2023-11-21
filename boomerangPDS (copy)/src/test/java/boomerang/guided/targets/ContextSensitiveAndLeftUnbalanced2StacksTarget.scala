package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalanced2StacksTarget {
  def main(args: Nothing*): Unit = {
    context()
  }

  private def context(): Unit = {
    val barParam = "bar"
    val bar = doPassArgument(barParam)
    val foo = doPassArgument("foo")
    val quz = doPassArgument("quz")
    new Nothing(bar)
    new Nothing(foo)
    new Nothing(quz)
  }

  private def doPassArgument(paramDoPassArgument: Nothing) = wrapped(paramDoPassArgument)

  private def wrapped(paramWrapped: Nothing) = new Nothing(paramWrapped)
}