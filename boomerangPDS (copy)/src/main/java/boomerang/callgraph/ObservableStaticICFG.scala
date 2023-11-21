package boomerang.callgraph

import boomerang.scene.CallGraph
import boomerang.scene.CallGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory

/**
 * An interprocedural control-flow graph, for which caller-callee edges can be observed using {@link
 * CalleeListener} and {@link CallerListener}. This call graph wraps a precomputed call graph and
 * notifies listeners about all interprocedual edges for the requested relation at once.
 *
 * @author Melanie Bruns on 04.05.2018
 */
object ObservableStaticICFG {
  private val LOGGER = LoggerFactory.getLogger(classOf[ObservableStaticICFG])
  private val IMPRECISE_CALL_GRAPH_WARN_THRESHOLD = 30000
}

class ObservableStaticICFG(
                            /** Wrapped static ICFG. If available, this is used to handle all queries. */
                            private var precomputedGraph: Nothing) extends Nothing {
  @Override def addCalleeListener(listener: Nothing): Unit = {
    val edges = precomputedGraph.edgesOutOf(listener.getObservedCaller)
    if (edges.size > ObservableStaticICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      ObservableStaticICFG.LOGGER.debug("Call graph has more than {} callees at {}", ObservableStaticICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD, listener.getObservedCaller)
      import scala.collection.JavaConversions._
      for (e <- edges) {
        ObservableStaticICFG.LOGGER.trace("\t callee {}", e.tgt)
      }
    }
    import scala.collection.JavaConversions._
    for (e <- edges) {
      listener.onCalleeAdded(listener.getObservedCaller, e.tgt)
    }
    if (edges.size eq 0) listener.onNoCalleeFound
  }

  @Override def addCallerListener(listener: Nothing): Unit = {
    val edges = precomputedGraph.edgesInto(listener.getObservedCallee)
    if (edges.size > ObservableStaticICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      ObservableStaticICFG.LOGGER.debug("Call graph has more than {} caller of {}", ObservableStaticICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD, listener.getObservedCallee)
      import scala.collection.JavaConversions._
      for (e <- edges) {
        ObservableStaticICFG.LOGGER.trace("\t callsite {}", e.src)
      }
    }
    import scala.collection.JavaConversions._
    for (e <- edges) {
      listener.onCallerAdded(e.src, listener.getObservedCallee)
    }
  }

  @Override def getStartPointsOf(m: Nothing): Nothing = m.getControlFlowGraph.getStartPoints

  @Override def isCallStmt(stmt: Nothing): Boolean = stmt.containsInvokeExpr

  @Override def isExitStmt(stmt: Nothing): Boolean = stmt.getMethod.getControlFlowGraph.getEndPoints.contains(stmt)

  @Override def isStartPoint(stmt: Nothing): Boolean = stmt.getMethod.getControlFlowGraph.getStartPoints.contains(stmt)

  @Override def getEndPointsOf(m: Nothing): Nothing = m.getControlFlowGraph.getEndPoints

  /**
   * Returns negative number to signify all edges are precomputed. CallGraphDebugger will add the
   * actual number in.
   *
   * @return -1 as all edges are precomputed, but we don't have access to the actual number
   */
  @Override def getNumberOfEdgesTakenFromPrecomputedGraph: Int = -1

  @Override def resetCallGraph(): Unit = {

    // Static call graph does not need to be reset, ignore this
  }

  @Override def computeFallback(): Unit = {

    // TODO Auto-generated method stub
  }

  @Override def addEdges(e: Nothing): Unit = {
    throw new Nothing("Unnecessary")
  }
}