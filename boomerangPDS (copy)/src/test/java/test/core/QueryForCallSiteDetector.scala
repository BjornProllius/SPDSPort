package test.core

import boomerang.BackwardQuery
import boomerang.Query
import boomerang.scene.AnalysisScope
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import boomerang.scene.jimple.AccessPathParser
import boomerang.scene.jimple.JimpleMethod
import boomerang.util.AccessPath
import java.util
import java.util.Collections
import java.util.Optional

class QueryForCallSiteDetector private[core](cg: Nothing) extends Nothing(cg) {
  private[core] val resultsMustNotBeEmpty = false
  private[core] val accessPathQuery = false
  private[core] val integerQueries = false
  private[core] val expectedAccessPaths = new Nothing

  private def getAllExpectedAccessPath(u: Nothing): Unit = {
    val arg = u.getStart.getInvokeExpr.getArg(1)
    if (arg.isStringConstant) {
      val value = arg.getStringValue
      expectedAccessPaths.addAll(AccessPathParser.parseAllFromString(value, u.getMethod.asInstanceOf[Nothing]))
    }
  }

  private class FirstArgumentOf(private var methodNameMatcher: Nothing) extends Nothing {
    @Override def test(stmt: Nothing): Nothing = {
      if (!stmt.getTarget.containsInvokeExpr) return Optional.empty
      val invokeExpr = stmt.getTarget.getInvokeExpr
      if (!invokeExpr.getMethod.getName.matches(methodNameMatcher)) return Optional.empty
      val param = invokeExpr.getArg(0)
      if (!param.isLocal) return Optional.empty
      val newBackwardQuery = BackwardQuery.make(stmt, param)
      Optional.of[Nothing](newBackwardQuery)
    }
  }

  @Override protected def generate(stmt: Nothing): Nothing = {
    var query = new QueryForCallSiteDetector#FirstArgumentOf("queryFor").test(stmt)
    if (query.isPresent) return Collections.singleton(query.get)
    query = new QueryForCallSiteDetector#FirstArgumentOf("queryForAndNotEmpty").test(stmt)
    if (query.isPresent) {
      resultsMustNotBeEmpty = true
      return Collections.singleton(query.get)
    }
    query = new QueryForCallSiteDetector#FirstArgumentOf("intQueryFor").test(stmt)
    if (query.isPresent) {
      integerQueries = true
      return Collections.singleton(query.get)
    }
    query = new QueryForCallSiteDetector#FirstArgumentOf("accessPathQueryFor").test(stmt)
    if (query.isPresent) {
      accessPathQuery = true
      getAllExpectedAccessPath(stmt)
      return Collections.singleton(query.get)
    }
    Collections.emptySet
  }
}