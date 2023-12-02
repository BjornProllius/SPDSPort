package boomerang.results

import boomerang.scene.ControlFlowGraph
import boomerang.scene.Val

trait AffectedLocation {

  def getStatement(): ControlFlowGraph.Edge

  def getVariable(): Val

  def getDataFlowPath(): List[PathElement]

  def getMessage(): String

  def getRuleIndex(): Int
}