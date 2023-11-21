package boomerang.callgraph

trait CalleeListener[N, M] {
  def getObservedCaller: N

  def onCalleeAdded(callSite: N, callee: M): Unit

  def onNoCalleeFound(): Unit
}