package boomerang.flowfunction

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.BackwardBoomerangSolver
import com.google.common.collect.Multimap
import java.util
import wpds.interfaces.State

trait IBackwardFlowFunction {
  /**
   * Called by the backward analysis, when the backward solver reaches the returnStmt (first
   * statement of callee method) of callee method with data-flow fact returnedVal.
   *
   * @param callee      The method the data-flow analysis returns from. The caller method is not
   *                    available, as it will be internally added by the framework.
   * @param returnStmt  The statement from which the method returns from (will be the first statement
   *                    of the callee method)
   * @param returnedVal The data-flow fact that is returned.
   * @return A set of data-flow facts (Val) which will be propagated at any call site of the callee
   *         method.
   */
  def returnFlow(callee: Nothing, returnStmt: Nothing, returnedVal: Nothing): Nothing

  /**
   * Called by the backward analysis, when the backward solver reaches the callSite. Will be invoked
   * once per available callee (in the call graph).
   *
   * @param callSite A call site reached by the backward analysis.
   * @param fact     The data-flow fact reaching the callSite
   * @param callee   The callee that may be invoked at the callSite
   * @param calleeSp The start point of callee (in the backward analysis, this typically will be
   *                 return or throw statements)
   * @return A set of data-flow facts (Val) that will be propagated at the calleeSp
   */
  def callFlow(callSite: Nothing, fact: Nothing, callee: Nothing, calleeSp: Nothing): Nothing

  /**
   * Called by the backward analysis, for any non return statements or call site statements. Note:
   * The logic differs from general IFDS logic here. edge.getTarget() can also contain a call site,
   * but fact is not used in the call site (no parameter or base variable of the call expression) .
   *
   * @param edge The control-flow graph edge that will be propagated next.
   * @param fact The incoming data-flow fact that reaches the edge.
   * @return A set of data-flow states (states in the pushdown system, typically of type
   *         Node<Edge,Val>).
   */
  def normalFlow(edge: Nothing, fact: Nothing): Nothing

  /**
   * Called by the backward analysis, when data-flow by-passes a call site with data-flow fact. Here
   * logic can be added to handle native calls, or methods that are excluded from propagation. Note:
   * The logic differs from general IFDS logic here. callToReturn is _only_ invoked when fact is
   * also used in the call site (edge.getTarget()), i.e. fact is a parameter or the base variable of
   * the call expression. As a consequence, special handling for call site may need to be
   * implemented as part of callToReturn and normalFlow.
   *
   * @param edge Edge that bypasses the call site. edge.getTarget() is the call site,
   *             edge.getStart() is any predecessor
   * @param fact The fact that by-passes the call site.
   * @return A set of data-flow states (states in the pushdown system, typically of type
   *         Node<Edge,Val>)
   */
  def callToReturnFlow(edge: Nothing, fact: Nothing): Nothing

  def setSolver(solver: Nothing, fieldLoadStatements: Nothing, fieldStoreStatements: Nothing): Unit
}