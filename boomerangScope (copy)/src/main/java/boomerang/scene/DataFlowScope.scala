package boomerang.scene

object DataFlowScope {
  val INCLUDE_ALL: DataFlowScope = new DataFlowScope() {
    @Override override def isExcluded(method: Nothing) = false

    @Override override def isExcluded(method: Nothing) = false
  }
}

trait DataFlowScope {
  def isExcluded(method: Nothing): Boolean

  def isExcluded(method: Nothing): Boolean
}