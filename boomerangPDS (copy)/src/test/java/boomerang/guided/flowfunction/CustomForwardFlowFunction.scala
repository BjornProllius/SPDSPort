package boomerang.guided.flowfunction

import boomerang.BoomerangOptions
import boomerang.ForwardQuery
import boomerang.flowfunction.DefaultForwardFlowFunction
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DeclaredMethod
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import java.util
import java.util.Collections
import wpds.interfaces.State

class CustomForwardFlowFunction(opts: Nothing) extends Nothing(opts) {
  @Override def callToReturnFlow(query: Nothing, edge: Nothing, fact: Nothing): Nothing = {
    if (edge.getStart.containsInvokeExpr) {
      // Avoid any propagations by passing the call site.
      if (declaredMethodIsSystemExit(edge.getStart)) return Collections.emptySet
    }
    super.callToReturnFlow(query, edge, fact)
  }

  @Override def normalFlow(query: Nothing, nextEdge: Nothing, fact: Nothing): Nothing = {
    if (nextEdge.getStart.containsInvokeExpr) {
      // Avoid any propagations by passing any call site (this covers the case, when the fact is not
      // used at the call site).
      if (declaredMethodIsSystemExit(nextEdge.getStart)) return Collections.emptySet
    }
    super.normalFlow(query, nextEdge, fact)
  }

  def declaredMethodIsSystemExit(callSite: Nothing): Boolean = {
    val method = callSite.getInvokeExpr.getMethod
    if (method.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System") && method.getName.equals("exit")) return true
    false
  }

  @Override def callFlow(callSite: Nothing, fact: Nothing, callee: Nothing): Nothing = {
    // Avoid propagations into the method when a call parameter reaches the call site
    if (callee.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System") && callee.getName.equals("exit")) return Collections.emptySet
    super.callFlow(callSite, fact, callee)
  }
}