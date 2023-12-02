package boomerang.guided.flowfunction

import boomerang.{BoomerangOptions, ForwardQuery, scene}
import boomerang.flowfunction.DefaultForwardFlowFunction
import boomerang.scene.{ControlFlowGraph, DeclaredMethod, Method, Statement, Val}
import wpds.interfaces.State

import java.util.Collections
import scala.collection.JavaConverters._

class CustomForwardFlowFunction(opts: BoomerangOptions) extends DefaultForwardFlowFunction(opts) {

  override def callToReturnFlow(query: ForwardQuery, edge: ControlFlowGraph.Edge, fact: Val): Collection[State] = {
    if (edge.getStart.containsInvokeExpr) {
      // Avoid any propagations by passing the call site.
      if (declaredMethodIsSystemExit(edge.getStart)) {
        return Collections.emptySet()
      }
    }
    super.callToReturnFlow(query, edge, fact)
  }

  override def normalFlow(query: ForwardQuery, nextEdge: ControlFlowGraph.Edge, fact: Val): Set[State] = {
    if (nextEdge.getStart.containsInvokeExpr) {
      // Avoid any propagations by passing any call site (this covers the case, when the fact is not
      // used at the call site).
      if (declaredMethodIsSystemExit(nextEdge.getStart)) {
        return Collections.emptySet()
      }
    }
    super.normalFlow(query, nextEdge, fact)
  }

  def declaredMethodIsSystemExit(callSite: Statement): Boolean = {
    val method: DeclaredMethod = callSite.getInvokeExpr.getMethod
    if (method.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System")
      && method.getName.equals("exit")) {
      return true
    }
    false
  }

  override def callFlow(callSite: Statement, fact: Val, callee: Method): Set[Val] = {
    // Avoid propagations into the method when a call parameter reaches the call site
    if (callee.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System")
      && callee.getName.equals("exit")) {
      return Collections.emptySet()
    }
    super.callFlow(callSite, fact, callee)
  }
}