package boomerang.guided.targets

import java.io.File

object WrappedInNewStringTarget {
  def main(args: Nothing*): Unit = {
    val x = new Nothing("bar")
    val bar = doPassArgument(x)
    new Nothing(bar)
  }

  def doPassArgument(param: Nothing): Nothing = param
}