package boomerang.guided.flowfunction

import boomerang.{DefaultBoomerangOptions, scene}
import boomerang.flowfunction.DefaultBackwardFlowFunction
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{DeclaredMethod, Method, Statement, Val}
import wpds.interfaces.State

import java.util.Collections
import scala.collection.JavaConverters._

class CustomBackwardFlowFunction(opts: DefaultBoomerangOptions) extends DefaultBackwardFlowFunction(opts) {

  override def normalFlow(edge: Edge, fact: Val): Collection[State] = {
    if (edge.getTarget.containsInvokeExpr) {
      val method: DeclaredMethod = edge.getTarget.getInvokeExpr.getMethod
      // Avoid any propagations by passing the call site (also when the fact is not used at the call
      // site).
      if (method.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System")
        && method.getName.equals("exit")) {
        return Collections.emptySet()
      }
    }
    super.normalFlow(edge, fact)
  }

  override def callToReturnFlow(edge: Edge, fact: Val): Collection[State] = {
    if (edge.getTarget.containsInvokeExpr) {
      val method: DeclaredMethod = edge.getTarget.getInvokeExpr.getMethod
      // Avoid any propagations by passing the call site (also when the fact is not used at the call
      // site).
      if (method.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System")
        && method.getName.equals("exit")) {
        return Collections.emptySet()
      }
    }
    super.callToReturnFlow(edge, fact)
  }

  override def callFlow(callSite: Statement, fact: Val, callee: Method, calleeSp: Statement): Collection[Val] = {
    // Avoid propagations into the method when a call parameter reaches the call site
    if (callee.getDeclaringClass.getFullyQualifiedName.equals("java.lang.System")
      && callee.getName.equals("exit")) {
      return Collections.emptySet()
    }
    super.callFlow(callSite, fact, callee, calleeSp)
  }
}