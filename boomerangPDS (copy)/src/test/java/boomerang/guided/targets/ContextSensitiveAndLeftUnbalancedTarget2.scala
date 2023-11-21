package boomerang.guided.targets

object ContextSensitiveAndLeftUnbalancedTarget2 {
  def main(args: Nothing*): Unit = {
    new ContextSensitiveAndLeftUnbalancedTarget2().context()
  }
}

class ContextSensitiveAndLeftUnbalancedTarget2 private {
  field = "bar"
  private var field: Nothing = null

  def context(): Unit = {
    val bytes = field.getBytes
    bytes.toString
  }
}