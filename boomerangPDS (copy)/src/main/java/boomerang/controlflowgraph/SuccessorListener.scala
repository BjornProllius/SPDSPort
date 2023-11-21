package boomerang.controlflowgraph

import boomerang.scene.Statement

abstract class SuccessorListener(private val curr: Nothing) {
  def getCurr: Nothing = curr

  def getSuccessor(succ: Nothing): Unit
}