package boomerang.guided.targets

import java.io.File

object BranchingAfterNewStringTest {

  def main(args: Array[String]): Unit = {
    val x = new String("bar")
    val y = new String("foo")
    val bar = doPassArgument(if (Math.random() > 0) x else y)
    new File(bar)
  }

  def doPassArgument(param: String): String = {
    val x = new String(param)
    println(x)
    x
  }
}