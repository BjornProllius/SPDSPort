package boomerang.controlflowgraph

import boomerang.scene.Statement

class StaticCFG extends ObservableControlFlowGraph {

  override def addPredsOfListener(l: PredecessorListener): Unit = {
    for (s <- l.getCurr.getMethod.getControlFlowGraph.getPredsOf(l.getCurr)) {
      l.getPredecessor(s)
    }
  }

  override def addSuccsOfListener(l: SuccessorListener): Unit = {
    for (s <- l.getCurr.getMethod.getControlFlowGraph.getSuccsOf(l.getCurr)) {
      l.getSuccessor(s)
    }
  }

  override def step(curr: Statement, succ: Statement): Unit = {}

  override def unregisterAllListeners(): Unit = {}
}