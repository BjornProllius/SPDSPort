package boomerang.callgraph

import boomerang.scene.{CallGraph, Method, Statement}
import org.slf4j.{Logger, LoggerFactory}

class ObservableStaticICFG(icfg: CallGraph) extends ObservableICFG[Statement, Method] {
  private val precomputedGraph: CallGraph = icfg
  private val LOGGER: Logger = LoggerFactory.getLogger(classOf[ObservableStaticICFG])
  private val IMPRECISE_CALL_GRAPH_WARN_THRESHOLD: Int = 30000

  override def addCalleeListener(listener: CalleeListener[Statement, Method]): Unit = {
    val edges: Collection[CallGraph.Edge] = precomputedGraph.edgesOutOf(listener.getObservedCaller())
    if (edges.size() > IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      LOGGER.debug(
        "Call graph has more than {} callees at {}",
        IMPRECISE_CALL_GRAPH_WARN_THRESHOLD,
        listener.getObservedCaller())
      for (e <- edges) {
        LOGGER.trace("\t callee {}", e.tgt())
      }
    }
    for (e <- edges) {
      listener.onCalleeAdded(listener.getObservedCaller(), e.tgt())
    }
    if (edges.size() == 0) {
      listener.onNoCalleeFound()
    }
  }

  override def addCallerListener(listener: CallerListener[Statement, Method]): Unit = {
    val edges: Collection[CallGraph.Edge] = precomputedGraph.edgesInto(listener.getObservedCallee())
    if (edges.size() > IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      LOGGER.debug(
        "Call graph has more than {} caller of {}",
        IMPRECISE_CALL_GRAPH_WARN_THRESHOLD,
        listener.getObservedCallee())
      for (e <- edges) {
        LOGGER.trace("\t callsite {}", e.src())
      }
    }
    for (e <- edges) {
      listener.onCallerAdded(e.src(), listener.getObservedCallee())
    }
  }

  override def getStartPointsOf(m: Method): Collection[Statement] = m.getControlFlowGraph().getStartPoints()

  override def isCallStmt(stmt: Statement): Boolean = stmt.containsInvokeExpr()

  override def isExitStmt(stmt: Statement): Boolean = stmt.getMethod().getControlFlowGraph().getEndPoints().contains(stmt)

  override def isStartPoint(stmt: Statement): Boolean = stmt.getMethod().getControlFlowGraph().getStartPoints().contains(stmt)

  override def getEndPointsOf(m: Method): Collection[Statement] = m.getControlFlowGraph().getEndPoints()

  override def getNumberOfEdgesTakenFromPrecomputedGraph(): Int = -1

  override def resetCallGraph(): Unit = {}

  override def computeFallback(): Unit = {}

  override def addEdges(e: CallGraph.Edge): Unit = throw new RuntimeException("Unnecessary")
}