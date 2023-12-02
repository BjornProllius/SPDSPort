package boomerang.guided.targets

import java.io.File

object PingPongTarget {

  def main(args: Array[String]): Unit = {
    val sb = new StringBuilder
    val hello = "hello"
    sb.append(hello).append("world")
    val result = sb.toString
    val file = new File(result)
  }
}