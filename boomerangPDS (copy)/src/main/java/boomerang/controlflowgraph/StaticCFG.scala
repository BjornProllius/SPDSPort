package boomerang.controlflowgraph

import boomerang.scene.Statement

class StaticCFG extends Nothing {
  @Override def addPredsOfListener(l: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (s <- l.getCurr.getMethod.getControlFlowGraph.getPredsOf(l.getCurr)) {
      l.getPredecessor(s)
    }
  }

  @Override def addSuccsOfListener(l: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (s <- l.getCurr.getMethod.getControlFlowGraph.getSuccsOf(l.getCurr)) {
      l.getSuccessor(s)
    }
  }

  @Override def step(curr: Nothing, succ: Nothing): Unit = {
  }

  @Override def unregisterAllListeners(): Unit = {
  }
}