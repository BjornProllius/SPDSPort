package boomerang.guided.targets

object CustomFlowFunctionTarget {
  def main(args: Nothing*): Unit = {
    val x = 1
    val y = x + 1
    val z = new Nothing
    System.exit(y)
    queryFor(z)
  }

  private def queryFor(x: Nothing): Unit = {
  }
}