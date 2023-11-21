package boomerang.callgraph

import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.scene.CallGraph
import boomerang.scene.CallGraph.Edge
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Statement
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory

/**
 * An interprocedural control-flow graph, for which caller-callee edges can be observed using {@link
 * CalleeListener} and {@link CallerListener}. Used for demand-driven call graph generation.
 *
 * <p>Starts with an graph only containing intraprocedual edges and uses a precomputed call graph to
 * derive callers.
 *
 * @author Melanie Bruns on 04.05.2018
 */
object ObservableDynamicICFG {
  private val logger = LoggerFactory.getLogger(classOf[ObservableDynamicICFG])
  private val IMPRECISE_CALL_GRAPH_WARN_THRESHOLD = 30
}

class ObservableDynamicICFG(private val cfg: Nothing, private val resolutionStrategy: Nothing) extends Nothing {
  private var numberOfEdgesTakenFromPrecomputedCallGraph = 0
  private val options = new Nothing
  private var demandDrivenCallGraph = new Nothing
  final private val calleeListeners = HashMultimap.create
  final private val callerListeners = HashMultimap.create

  @Override def addCalleeListener(listener: Nothing): Unit = {
    if (!calleeListeners.put(listener.getObservedCaller, listener)) return
    // Notify the new listener about edges we already know
    val stmt = listener.getObservedCaller
    val edges = demandDrivenCallGraph.edgesOutOf(stmt)
    if (edges.size > ObservableDynamicICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      ObservableDynamicICFG.logger.debug("Call graph has more than {} callees at {}", ObservableDynamicICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD, listener.getObservedCaller)
      import scala.collection.JavaConversions._
      for (e <- Lists.newArrayList(edges)) {
        ObservableDynamicICFG.logger.trace("\t callee {}", e.tgt)
      }
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(edges)) {
      listener.onCalleeAdded(stmt, e.tgt)
    }
    val ie = stmt.getInvokeExpr
    // Now check if we need to find new edges
    if (ie.isInstanceInvokeExpr) {
      // If it was invoked on an object we might find new instances
      if (ie.isSpecialInvokeExpr) addCallIfNotInGraph(stmt, resolutionStrategy.resolveSpecialInvoke(ie))
      else {
        // Query for callees of the unit and add edges to the graph
        import scala.collection.JavaConversions._
        for (method <- resolutionStrategy.resolveInstanceInvoke(stmt)) {
          addCallIfNotInGraph(stmt, method)
        }
      }
    }
    else {
      // Call was not invoked on an object. Must be static
      addCallIfNotInGraph(stmt, resolutionStrategy.resolveStaticInvoke(ie))
    }
  }

  @Override def addCallerListener(listener: Nothing): Unit = {
    if (!callerListeners.put(listener.getObservedCallee, listener)) return
    val method = listener.getObservedCallee
    ObservableDynamicICFG.logger.debug("Queried for callers of {}.", method)
    // Notify the new listener about what we already now
    val edges = demandDrivenCallGraph.edgesInto(method)
    if (edges.size > ObservableDynamicICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      ObservableDynamicICFG.logger.debug("Call graph has more than {} caller of {}", ObservableDynamicICFG.IMPRECISE_CALL_GRAPH_WARN_THRESHOLD, listener.getObservedCallee)
      import scala.collection.JavaConversions._
      for (e <- edges) {
        ObservableDynamicICFG.logger.trace("\t callsite {}", e.src)
      }
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(edges)) {
      listener.onCallerAdded(e.src, method)
    }
  }

  /**
   * Returns true if the call was added to the call graph, false if it was already present and the
   * call graph did not change
   */
  protected def addCallIfNotInGraph(caller: Nothing, callee: Nothing): Boolean = {
    val edge = new Nothing(caller, callee)
    if (!demandDrivenCallGraph.addEdge(edge)) return false
    ObservableDynamicICFG.logger.debug("Added call from unit '{}' to method '{}'", caller, callee)
    // Notify all interested listeners, so ..
    // .. CalleeListeners interested in callees of the caller or the CallGraphExtractor that is
    // interested in any
    import scala.collection.JavaConversions._
    for (listener <- Lists.newArrayList(calleeListeners.get(caller))) {
      listener.onCalleeAdded(caller, callee)
    }
    // .. CallerListeners interested in callers of the callee or the CallGraphExtractor that is
    // interested in any
    import scala.collection.JavaConversions._
    for (listener <- Lists.newArrayList(callerListeners.get(callee))) {
      listener.onCallerAdded(caller, callee)
    }
    true
  }

  protected def notifyNoCalleeFound(s: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (l <- Lists.newArrayList(calleeListeners.get(s))) {
      l.onNoCalleeFound
    }
  }

  @Override def isCallStmt(unit: Nothing): Boolean = unit.containsInvokeExpr

  @Override def isExitStmt(unit: Nothing): Boolean = unit.getMethod.getControlFlowGraph.getEndPoints.contains(unit)

  @Override def isStartPoint(unit: Nothing): Boolean = unit.getMethod.getControlFlowGraph.getStartPoints.contains(unit)

  @Override def getStartPointsOf(m: Nothing): Nothing = m.getControlFlowGraph.getStartPoints

  @Override def getEndPointsOf(m: Nothing): Nothing = m.getControlFlowGraph.getEndPoints

  @Override def getNumberOfEdgesTakenFromPrecomputedGraph: Int = numberOfEdgesTakenFromPrecomputedCallGraph

  @Override def resetCallGraph(): Unit = {
    demandDrivenCallGraph = new Nothing
    numberOfEdgesTakenFromPrecomputedCallGraph = 0
    calleeListeners.clear
    callerListeners.clear
  }

  @Override def computeFallback(): Unit = {
    resolutionStrategy.computeFallback(this)
  }

  @Override def addEdges(e: Nothing): Unit = {
    demandDrivenCallGraph.addEdge(e)
  }
}