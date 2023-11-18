package boomerang.controlflowgraph

import boomerang.scene.Statement

trait ObservableControlFlowGraph {

  def addPredsOfListener(l: PredecessorListener): Unit

  def addSuccsOfListener(l: SuccessorListener): Unit

  def step(curr: Statement, succ: Statement): Unit

  def unregisterAllListeners(): Unit
}