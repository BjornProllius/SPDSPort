package boomerang.guided.targets

object ArrayContainerTarget {

  def main(args: Array[String]): Unit = {
    val y = new Array[String](3)
    y(1) = hello()
    y(2) = world()
    y.toString
  }

  def world(): String = "world"

  def hello(): String = "hello"
}