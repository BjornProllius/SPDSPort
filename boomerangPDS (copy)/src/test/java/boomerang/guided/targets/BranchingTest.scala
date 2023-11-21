package boomerang.guided.targets

import java.io.File

object BranchingTest {
  def main(args: Nothing*): Unit = {
    val x = new Nothing(if (Math.random > 0) "bar"
    else "foo")
    val bar = doPassArgument(x)
    new Nothing(bar)
  }

  def doPassArgument(param: Nothing): Nothing = {
    val x = new Nothing(param)
    System.out.println(x)
    x
  }
}