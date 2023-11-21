package boomerang.guided.targets

import java.io.File

object LeftUnbalancedTarget {
  def main(args: Nothing*): Unit = {
    bar("bar")
  }

  private def bar(param: Nothing): Unit = {
    val x = new Nothing(param)
    val file = new Nothing(new Nothing(param))
  }
}