package boomerang.guided

import boomerang.{BackwardQuery, ForwardQuery, Query}
import boomerang.scene.{ControlFlowGraph, Val}
import java.util.Collection

trait IDemandDrivenGuidedManager {
  def onForwardFlow(query: ForwardQuery, dataFlowEdge: ControlFlowGraph.Edge, dataFlowVal: Val): Collection[Query]

  def onBackwardFlow(query: BackwardQuery, dataFlowEdge: ControlFlowGraph.Edge, dataFlowVal: Val): Collection[Query]
}