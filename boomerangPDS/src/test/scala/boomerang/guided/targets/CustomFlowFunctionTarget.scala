package boomerang.guided.targets

object CustomFlowFunctionTarget {

  def main(args: Array[String]): Unit = {
    val x = 1
    val y = x + 1
    val z = new Object()
    System.exit(y)
    queryFor(z)
  }

  private def queryFor(x: Any): Unit = {}
}