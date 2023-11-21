package boomerang.guided.targets

import java.io.File

object BranchingAfterNewStringTest {
  def main(args: Nothing*): Unit = {
    val x = new Nothing("bar")
    val y = new Nothing("foo")
    val bar = doPassArgument(if (Math.random > 0) x
    else y)
    new Nothing(bar)
  }

  def doPassArgument(param: Nothing): Nothing = {
    val x = new Nothing(param)
    System.out.println(x)
    x
  }
}