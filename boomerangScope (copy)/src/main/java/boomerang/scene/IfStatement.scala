package boomerang.scene

object IfStatement {
  object Evaluation extends Enumeration {
    type Evaluation = Value
    val TRUE, FALSE, UNKOWN = Value
  }
}

trait IfStatement {
  def getTarget: Nothing

  def evaluate(`val`: Nothing): IfStatement.Evaluation

  def uses(`val`: Nothing): Boolean
}