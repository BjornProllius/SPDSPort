package boomerang.guided.targets

import java.io.File

object BasicTarget {

  def main(args: Array[String]): Unit = {
    val x = "bar"
    new File(x)
  }
}