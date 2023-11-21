package boomerang.guided

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import java.util

trait IDemandDrivenGuidedManager {
  def onForwardFlow(query: Nothing, dataFlowEdge: Nothing, dataFlowVal: Nothing): Nothing

  def onBackwardFlow(query: Nothing, dataFlowEdge: Nothing, dataFlowVal: Nothing): Nothing
}