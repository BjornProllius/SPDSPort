package boomerang.controlflowgraph

import boomerang.scene.Statement

trait ObservableControlFlowGraph {
  def addPredsOfListener(l: Nothing): Unit

  def addSuccsOfListener(l: Nothing): Unit

  def step(curr: Nothing, succ: Nothing): Unit

  def unregisterAllListeners(): Unit
}