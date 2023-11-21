package test.core

import boomerang.ForwardQuery
import boomerang.Query
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import boomerang.scene.Type
import boomerang.scene.Val
import java.util.Optional

class AllocationSiteOf(private var `type`: Nothing) extends Nothing {
  def test(cfgEdge: Nothing): Nothing = {
    val stmt = cfgEdge.getStart
    if (stmt.isAssign) if (stmt.getLeftOp.isLocal && stmt.getRightOp.isNewExpr) {
      val expr = stmt.getRightOp.getNewExprType
      if (expr.isSubtypeOf(`type`)) {
        val local = stmt.getLeftOp
        val forwardQuery = new Nothing(cfgEdge, new Nothing(local, stmt, stmt.getRightOp))
        return Optional.of[Nothing](forwardQuery)
      }
    }
    Optional.empty
  }
}