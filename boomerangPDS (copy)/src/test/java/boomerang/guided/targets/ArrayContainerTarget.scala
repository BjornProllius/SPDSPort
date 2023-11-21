package boomerang.guided.targets

object ArrayContainerTarget {
  def main(args: Nothing*): Unit = {
    val y = new Array[Nothing](2)
    y(1) = hello
    y(2) = world
    y.toString
  }

  def world = "world"

  def hello = "hello"
}