package boomerang.guided

import boomerang.{BackwardQuery, ForwardQuery, Query}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{Statement, Val}

import scala.collection.JavaConverters._

class ArrayContainerCollectionManager extends IDemandDrivenGuidedManager {

  override def onForwardFlow(query: ForwardQuery, dataFlowEdge: Edge, dataFlowVal: Val): Collection[Query] = {
    val targetStmt = dataFlowEdge.getStart
    // Any statement of type someVariable[..] = rightOp
    if (targetStmt.isAssign && targetStmt.getLeftOp.isArrayRef) {
      // If propagated fact also matches "someVariable"
      if (targetStmt.getLeftOp.getArrayBase.getX == dataFlowVal) {
        // Do start a new backward query for rightOp
        return Set(BackwardQuery.make(dataFlowEdge, targetStmt.getRightOp)).asJava
      }
    }
    Set.empty[Query].asJava
  }

  override def onBackwardFlow(query: BackwardQuery, dataFlowEdge: Edge, dataFlowVal: Val): Collection[Query] = {
    Set.empty[Query].asJava
  }
}