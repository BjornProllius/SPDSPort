package boomerang.guided.targets

import java.io.File

object ContextSensitiveTarget {
  def main(args: Nothing*): Unit = {
    val bar = doPassArgument("bar")
    val foo = doPassArgument("foo")
    val quz = doPassArgument("quz")
    new Nothing(bar)
    new Nothing(foo)
    new Nothing(quz)
  }

  private def doPassArgument(param: Nothing) = new Nothing(param)
}