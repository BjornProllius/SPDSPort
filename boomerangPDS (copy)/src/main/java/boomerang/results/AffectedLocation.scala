package boomerang.results

import boomerang.scene.ControlFlowGraph
import boomerang.scene.Val
import java.util

trait AffectedLocation {
  def getStatement: Nothing

  def getVariable: Nothing

  def getDataFlowPath: Nothing

  def getMessage: Nothing

  def getRuleIndex: Int
}