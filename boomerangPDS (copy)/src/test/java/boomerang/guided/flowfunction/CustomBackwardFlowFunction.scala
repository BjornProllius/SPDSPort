package boomerang.guided.flowfunction

import boomerang.DefaultBoomerangOptions
import boomerang.flowfunction.DefaultBackwardFlowFunction
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DeclaredMethod
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import java.util
import java.util.Collections
import wpds.interfaces.State

class CustomBackwardFlowFunction(opts: Nothing) extends Nothing(opts) {
  @Override def normalFlow(edge: Nothing, fact: Nothing): Nothing = {
    if (edge.getTarget.containsInvokeExpr) {
      val method = edge.getTarget.getInvokeExpr.getMethod
      // Avoid any propagations by passing the call site (also when the fact is not used at the call
      // site).
      if (method.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System") && method.getName.equals("exit")) return Collections.emptySet
    }
    super.normalFlow(edge, fact)
  }

  @Override def callToReturnFlow(edge: Nothing, fact: Nothing): Nothing = {
    if (edge.getTarget.containsInvokeExpr) {
      val method = edge.getTarget.getInvokeExpr.getMethod
      // Avoid any propagations by passing the call site (also when the fact is not used at the call
      // site).
      if (method.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System") && method.getName.equals("exit")) return Collections.emptySet
    }
    super.callToReturnFlow(edge, fact)
  }

  @Override def callFlow(callSite: Nothing, fact: Nothing, callee: Nothing, calleeSp: Nothing): Nothing = {
    // Avoid propagations into the method when a call parameter reaches the call site
    if (callee.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System") && callee.getName.equals("exit")) return Collections.emptySet
    super.callFlow(callSite, fact, callee, calleeSp)
  }
}