package boomerang.guided

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import boomerang.scene.Val
import java.util
import java.util.Collections

class ArrayContainerCollectionManager extends Nothing {
  @Override def onForwardFlow(query: Nothing, dataFlowEdge: Nothing, dataFlowVal: Nothing): Nothing = {
    val targetStmt = dataFlowEdge.getStart
    // Any statement of type someVariable[..] = rightOp
    if (targetStmt.isAssign && targetStmt.getLeftOp.isArrayRef) {
      // If propagated fact also matches "someVariable"
      if (targetStmt.getLeftOp.getArrayBase.getX.equals(dataFlowVal)) {
        // Do start a new backward query for rightOp
        return Collections.singleton(BackwardQuery.make(dataFlowEdge, targetStmt.getRightOp))
      }
    }
    Collections.emptySet
  }

  @Override def onBackwardFlow(query: Nothing, dataFlowEdge: Nothing, dataFlowVal: Nothing): Nothing = Collections.emptySet
}