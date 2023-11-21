package boomerang.controlflowgraph

import boomerang.scene.Statement

abstract class PredecessorListener(private val curr: Nothing) {
  def getCurr: Nothing = curr

  def getPredecessor(pred: Nothing): Unit
}