package boomerang.guided.targets

import java.io.File

object WrappedInNewStringInnerTarget {

  def main(args: Array[String]): Unit = {
    val x = "bar"
    val bar = doPassArgument(x)
    new File(bar)
  }

  def doPassArgument(param: String): String = {
    val x = new String(param)
    println(x)
    x
  }
}