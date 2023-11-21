package boomerang.guided.targets

object CustomFlowFunctionIntTarget {
  def main(args: Nothing*): Unit = {
    var z = 0
    if (z == 0) z += 1
    System.out.println(z)
    System.exit(0)
    queryFor(z)
  }

  private def queryFor(x: Int): Unit = {
  }
}