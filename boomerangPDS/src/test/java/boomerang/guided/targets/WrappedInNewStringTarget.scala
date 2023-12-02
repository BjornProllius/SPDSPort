package boomerang.guided.targets

import java.io.File

object WrappedInNewStringTarget {

  def main(args: Array[String]): Unit = {
    val x = new String("bar")
    val bar = doPassArgument(x)
    new File(bar)
  }

  def doPassArgument(param: String): String = {
    param
  }
}