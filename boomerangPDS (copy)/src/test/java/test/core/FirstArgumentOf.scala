package test.core

import boomerang.BackwardQuery
import boomerang.Query
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import java.util.Optional

class FirstArgumentOf(private var methodNameMatcher: Nothing) extends Nothing {
  @Override def test(stmt: Nothing): Nothing = {
    if (!stmt.getStart.containsInvokeExpr) return Optional.empty
    val invokeExpr = stmt.getStart.getInvokeExpr
    if (!invokeExpr.getMethod.getName.matches(methodNameMatcher)) return Optional.empty
    val param = invokeExpr.getArg(0)
    if (!param.isLocal) return Optional.empty
    val newBackwardQuery = BackwardQuery.make(stmt, param)
    Optional.of[Nothing](newBackwardQuery)
  }
}