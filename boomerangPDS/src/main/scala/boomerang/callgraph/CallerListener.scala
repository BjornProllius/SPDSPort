package boomerang.callgraph

trait CallerListener[N, M] {
  def getObservedCallee(): M

  def onCallerAdded(callSite: N, callee: M): Unit
}