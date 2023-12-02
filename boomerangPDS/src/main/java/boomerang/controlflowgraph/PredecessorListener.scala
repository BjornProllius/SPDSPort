package boomerang.controlflowgraph

import boomerang.scene.Statement

abstract class PredecessorListener(curr: Statement) {

  def getCurr: Statement = curr

  def getPredecessor(pred: Statement): Unit
}