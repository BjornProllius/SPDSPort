package boomerang.guided.targets

object IntegerCastTarget {

  def main(args: Array[String]): Unit = {
    val x = 1
    val y = castToObject(x)
    y.toString
  }

  def castToObject(param: Any): Any = param
}