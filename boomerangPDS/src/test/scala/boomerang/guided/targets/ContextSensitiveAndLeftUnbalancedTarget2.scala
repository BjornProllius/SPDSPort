package boomerang.guided.targets

object ContextSensitiveAndLeftUnbalancedTarget2 {

  def main(args: Array[String]): Unit = {
    new ContextSensitiveAndLeftUnbalancedTarget2().context()
  }
}

class ContextSensitiveAndLeftUnbalancedTarget2 {

  private val field: String = "bar"

  def context(): Unit = {
    val bytes = field.getBytes
    bytes.toString
  }
}