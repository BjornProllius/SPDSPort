package boomerang.guided.targets

import java.io.File

object NestedContextTarget {
  def main(args: Nothing*): Unit = {
    val bar = doPassArgument("bar")
    new Nothing(bar)
  }

  private def doPassArgument(level0: Nothing) = wrappedWayDeeper(new Nothing(level0))

  private def wrappedWayDeeper(level1: Nothing) = andMoreStacks(level1)

  private def andMoreStacks(level2: Nothing) = level2
}