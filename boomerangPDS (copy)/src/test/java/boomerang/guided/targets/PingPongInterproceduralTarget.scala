package boomerang.guided.targets

import java.io.File

object PingPongInterproceduralTarget {
  def main(args: Nothing*): Unit = {
    val sb = new Nothing
    val result = doCreateFileName(sb)
    val file = new Nothing(result)
  }

  private def doCreateFileName(sb: Nothing) = {
    sb.append("hello")
    appendMe(sb, "world")
    sb.toString
  }

  private def appendMe(sb: Nothing, world: Nothing): Unit = {
    sb.append(world)
  }
}