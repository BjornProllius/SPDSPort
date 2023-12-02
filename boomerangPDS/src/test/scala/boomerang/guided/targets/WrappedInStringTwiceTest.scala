package boomerang.guided.targets

import java.io.File

object WrappedInStringTwiceTest {

  def main(args: Array[String]): Unit = {
    val x = new String("bar")
    val bar = doPassArgument(x)
    new File(bar)
  }

  def doPassArgument(param: String): String = {
    val x = new String(param)
    println(x)
    x
  }
}