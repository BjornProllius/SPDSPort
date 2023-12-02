package boomerang.controlflowgraph

import boomerang.scene.Statement

abstract class SuccessorListener(curr: Statement) {

  def getCurr: Statement = curr

  def getSuccessor(succ: Statement): Unit
}