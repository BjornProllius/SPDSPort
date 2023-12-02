package boomerang.guided.targets

import java.io.File

object LeftUnbalancedTarget {

  def main(args: Array[String]): Unit = {
    bar("bar")
  }

  private def bar(param: String): Unit = {
    val x = new String(param)
    val file = new File(new String(param))
  }
}