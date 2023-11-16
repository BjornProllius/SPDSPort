package boomerang.callgraph

import boomerang.scene.CallGraph.Edge

/**
 * An interprocedural control-flow graph, for which caller-callee edges can be observed using {@link
 * CalleeListener} and {@link CallerListener}. Can be used for demand-driven call graph generation.
 *
 * @param <N> Nodes in the CFG, typically {@link Unit} or {@link Block}
 * @param <M> Method representation
 * @author Melanie Bruns on 04.05.2018
 */

trait ObservableICFG[N, M] {
  /** Registers a listener that will be notified whenever a callee is added */
  def addCalleeListener(listener: CalleeListener[N, M]): Unit
  /** Registers a listener that will be notified whenever a caller is added. */
  def addCallerListener(listener: CallerListener[N, M]): Unit
  /** Returns <code>true</code> if the given statement is a call site. */
  def isCallStmt(stmt: N): Boolean

  /**
   * Returns <code>true</code> if the given statement leads to a method return (exceptional or not).
   * For backward analyses may also be start statements.
   */
  def isExitStmt(stmt: N): Boolean

  /**
   * Returns true is this is a method's start statement. For backward analyses those may also be
   * return or throws statements.
   */
  def isStartPoint(stmt: N): Boolean

  def getNumberOfEdgesTakenFromPrecomputedGraph(): Int

  /**
   * Resets the call graph. Only affects the call graph if it was built demand-driven, otherwise
   * graph will remain unchanged. Demand-driven call graph will keep intraprocedual information, but
   * reset start with an empty call graph again.
   */
  def resetCallGraph(): Unit

  def getStartPointsOf(callee: M): Collection[N]

  def getEndPointsOf(flowReaches: M): Collection[N]

  def computeFallback(): Unit

  def addEdges(e: Edge): Unit
}