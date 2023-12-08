package boomerang.guided.targets

object CustomFlowFunctionIntTarget {

  def main(args: Array[String]): Unit = {
    var z = 0
    if (z == 0) z += 1
    println(z)
    System.exit(0)
    queryFor(z)
  }

  private def queryFor(x: Int): Unit = {}
}