package boomerang.guided.targets

import java.io.File

object WrappedInNewStringInnerTarget {
  def main(args: Nothing*): Unit = {
    val x = "bar"
    val bar = doPassArgument(x)
    new Nothing(bar)
  }

  def doPassArgument(param: Nothing): Nothing = {
    val x = new Nothing(param)
    System.out.println(x)
    x
  }
}