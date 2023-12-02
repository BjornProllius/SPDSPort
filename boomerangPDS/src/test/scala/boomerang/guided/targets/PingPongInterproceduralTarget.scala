package boomerang.guided.targets

import java.io.File

object PingPongInterproceduralTarget {

  def main(args: Array[String]): Unit = {
    val sb = new StringBuilder
    val result = doCreateFileName(sb)
    val file = new File(result)
  }

  private def doCreateFileName(sb: StringBuilder): String = {
    sb.append("hello")
    appendMe(sb, "world")
    sb.toString
  }

  private def appendMe(sb: StringBuilder, world: String): Unit = {
    sb.append(world)
  }
}