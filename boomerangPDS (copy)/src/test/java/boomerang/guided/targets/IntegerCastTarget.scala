package boomerang.guided.targets

object IntegerCastTarget {
  def main(args: Nothing*): Unit = {
    val x = 1
    val y = castToObject(x)
    y.toString
  }

  def castToObject(param: Nothing): Nothing = param
}