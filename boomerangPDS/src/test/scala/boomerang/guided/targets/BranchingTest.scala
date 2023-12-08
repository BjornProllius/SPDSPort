package boomerang.guided.targets

import java.io.File

object BranchingTest {

  def main(args: Array[String]): Unit = {
    val x = if (Math.random() > 0) new String("bar") else new String("foo")
    val bar = doPassArgument(x)
    new File(bar)
  }

  def doPassArgument(param: String): String = {
    val x = new String(param)
    println(x)
    x
  }
}