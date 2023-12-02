package boomerang.guided.targets

import java.io.File

object ContextSensitiveAndLeftUnbalanced2StacksTarget {

  def main(args: Array[String]): Unit = {
    context()
  }

  private def context(): Unit = {
    val barParam = "bar"
    val bar = doPassArgument(barParam)
    val foo = doPassArgument("foo")
    val quz = doPassArgument("quz")
    new File(bar)
    new File(foo)
    new File(quz)
  }

  private def doPassArgument(paramDoPassArgument: String): String = {
    wrapped(paramDoPassArgument)
  }

  private def wrapped(paramWrapped: String): String = {
    new String(paramWrapped)
  }
}