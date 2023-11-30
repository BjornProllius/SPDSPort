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
import java.util.Collection
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
class ObservableDynamicICFG extends ObservableICFG[Statement, Method] {

  private val logger = LoggerFactory.getLogger(classOf[ObservableDynamicICFG])

  private val IMPRECISE_CALL_GRAPH_WARN_THRESHOLD = 30

  private var numberOfEdgesTakenFromPrecomputedCallGraph = 0

  private var options = new CallGraphOptions()
  private var demandDrivenCallGraph = new CallGraph()

  private val calleeListeners = HashMultimap.create[Statement, CalleeListener[Statement, Method]]()
  private val callerListeners = HashMultimap.create[Method, CallerListener[Statement, Method]]()

  private val cfg: ObservableControlFlowGraph
  private val resolutionStrategy: ICallerCalleeResolutionStrategy

  def this(cfg: ObservableControlFlowGraph, resolutionStrategy: ICallerCalleeResolutionStrategy) {
    this.cfg = cfg
    this.resolutionStrategy = resolutionStrategy
  }

  override def addCalleeListener(listener: CalleeListener[Statement, Method]): Unit = {
    if (!calleeListeners.put(listener.getObservedCaller(), listener)) {
      return
    }

    val stmt = listener.getObservedCaller()
    val edges = demandDrivenCallGraph.edgesOutOf(stmt)

    if (edges.size() > IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      logger.debug(
        "Call graph has more than {} callees at {}",
        IMPRECISE_CALL_GRAPH_WARN_THRESHOLD,
        listener.getObservedCaller())
      for (e <- Lists.newArrayList(edges)) {
        logger.trace("\t callee {}", e.tgt())
      }
    }

    for (e <- Lists.newArrayList(edges)) {
      listener.onCalleeAdded(stmt, e.tgt())
    }

    val ie = stmt.getInvokeExpr()

    if ((ie.isInstanceInvokeExpr())) {
      if (ie.isSpecialInvokeExpr()) {
        addCallIfNotInGraph(stmt, resolutionStrategy.resolveSpecialInvoke(ie))
      } else {
        for (method <- resolutionStrategy.resolveInstanceInvoke(stmt)) {
          addCallIfNotInGraph(stmt, method)
        }
      }
    } else {
      addCallIfNotInGraph(stmt, resolutionStrategy.resolveStaticInvoke(ie))
    }
  }

  override def addCallerListener(listener: CallerListener[Statement, Method]): Unit = {
    if (!callerListeners.put(listener.getObservedCallee(), listener)) {
      return
    }

    val method = listener.getObservedCallee()

    logger.debug("Queried for callers of {}.", method)

    val edges = demandDrivenCallGraph.edgesInto(method)
    if (edges.size() > IMPRECISE_CALL_GRAPH_WARN_THRESHOLD) {
      logger.debug(
        "Call graph has more than {} caller of {}",
        IMPRECISE_CALL_GRAPH_WARN_THRESHOLD,
        listener.getObservedCallee())
      for (e <- edges) {
        logger.trace("\t callsite {}", e.src())
      }
    }
    for (e <- Lists.newArrayList(edges)) {
      listener.onCallerAdded(e.src(), method)
    }
  }

  protected def addCallIfNotInGraph(caller: Statement, callee: Method): Boolean = {
    val edge = new Edge(caller, callee)
    if (!demandDrivenCallGraph.addEdge(edge)) {
      return false
    }
    logger.debug("Added call from unit '{}' to method '{}'", caller, callee)
    for (listener <- Lists.newArrayList(calleeListeners.get(caller))) {
      listener.onCalleeAdded(caller, callee)
    }
    for (listener <- Lists.newArrayList(callerListeners.get(callee))) {
      listener.onCallerAdded(caller, callee)
    }
    true
  }

  protected def notifyNoCalleeFound(s: Statement): Unit = {
    for (l <- Lists.newArrayList(calleeListeners.get(s))) {
      l.onNoCalleeFound()
    }
  }

  override def isCallStmt(unit: Statement): Boolean = {
    unit.containsInvokeExpr()
  }

  override def isExitStmt(unit: Statement): Boolean = {
    unit.getMethod().getControlFlowGraph().getEndPoints().contains(unit)
  }

  override def isStartPoint(unit: Statement): Boolean = {
    unit.getMethod().getControlFlowGraph().getStartPoints().contains(unit)
  }

  override def getStartPointsOf(m: Method): Collection[Statement] = {
    m.getControlFlowGraph().getStartPoints()
  }

  override def getEndPointsOf(m: Method): Collection[Statement] = {
    m.getControlFlowGraph().getEndPoints()
  }

  override def getNumberOfEdgesTakenFromPrecomputedGraph(): Int = {
    numberOfEdgesTakenFromPrecomputedCallGraph
  }

  override def resetCallGraph(): Unit = {
    demandDrivenCallGraph = new CallGraph()
    numberOfEdgesTakenFromPrecomputedCallGraph = 0
    calleeListeners.clear()
    callerListeners.clear()
  }

  override def computeFallback(): Unit = {
    resolutionStrategy.computeFallback(this)
  }

  override def addEdges(e: Edge): Unit = {
    demandDrivenCallGraph.addEdge(e)
  }
}
