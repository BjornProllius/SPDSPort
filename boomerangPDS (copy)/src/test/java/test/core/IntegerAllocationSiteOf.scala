package test.core

import boomerang.ForwardQuery
import boomerang.Query
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import boomerang.scene.Val
import java.util.Optional

class IntegerAllocationSiteOf extends Nothing {
  def test(cfgEdge: Nothing): Nothing = {
    val stmt = cfgEdge.getStart
    if (stmt.isAssign) if (stmt.getLeftOp.toString.contains("allocation")) if (stmt.getLeftOp.isLocal && stmt.getRightOp.isIntConstant) {
      val local = stmt.getLeftOp
      val forwardQuery = new Nothing(cfgEdge, local)
      return Optional.of(forwardQuery)
    }
    Optional.empty
  }
}