package boomerang.guided.targets

import java.io.File

object PingPongTarget {
  def main(args: Nothing*): Unit = {
    val sb = new Nothing
    val hello = "hello"
    sb.append(hello).append("world")
    val result = sb.toString
    val file = new Nothing(result)
  }
}