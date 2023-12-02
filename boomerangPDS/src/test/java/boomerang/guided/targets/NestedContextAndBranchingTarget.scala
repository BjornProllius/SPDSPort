package boomerang.guided.targets

import java.io.File

object NestedContextAndBranchingTarget {

  def main(args: Array[String]): Unit = {
    val bar = doPassArgument("bar")
    new File(bar)
  }

  private def doPassArgument(level0: String): String = {
    wrappedWayDeeper(new String(level0))
  }

  private def wrappedWayDeeper(level1: String): String = {
    if (Math.random() > 0) {
      "foo"
    } else {
      andMoreStacks(level1)
    }
  }

  private def andMoreStacks(level2: String): String = {
    level2
  }
}