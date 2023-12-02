package boomerang.guided.targets

object ValueOfTarget {

  def main(args: Array[String]): Unit = {
    val z = 3
    val x = 1
    foo(x, z)
  }

  private def foo(x: Int, z: Int): Unit = {
    Integer.valueOf(z)
    Query.queryFor(x)
  }
}