package boomerang.callgraph

import boomerang._
import boomerang.scene._
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.solver._
import com.google.common.collect._
import org.slf4j._
import sync.pds.solver.nodes._
import wpds.impl.Weight
import scala.jdk.CollectionConverters._
import scala.collection.mutable

class BoomerangResolver extends ICallerCalleeResolutionStrategy {
  object BoomerangResolver {
    val FACTORY: (WeightedBoomerang[_ <: Weight], CallGraph) => BoomerangResolver = (solver, cg) => new BoomerangResolver(solver, cg)
    private val logger = LoggerFactory.getLogger(classOf[BoomerangResolver])

    object NoCalleeFoundFallbackOptions extends Enumeration {
      type NoCalleeFoundFallbackOptions = Value
      val PRECOMPUTED, BYPASS = Value
    }

    private val THREAD_CLASS = "java.lang.Thread"
    private val THREAD_START_SIGNATURE = "<java.lang.Thread: void start()>"
    private val THREAD_RUN_SUB_SIGNATURE = "void run()"

    private var FALLBACK_OPTION = NoCalleeFoundFallbackOptions.BYPASS
    private val didNotFindMethodLog: Multimap[DeclaredMethod, WrappedClass] = HashMultimap.create()
  }

  class BoomerangResolver(cg: CallGraph, scope: DataFlowScope) {
    import BoomerangResolver._

    private val solver = new Boomerang(cg, scope)
    private val precomputedCallGraph = cg
    private val queriedInvokeExprAndAllocationSitesFound = Sets.newHashSet[Statement]()
    private val queriedInvokeExpr = Sets.newHashSet[Statement]()

    def this(solver: WeightedBoomerang[_ <: Weight], initialCallGraph: CallGraph) {
      this(solver, true, initialCallGraph)
    }

    def this(solver: WeightedBoomerang[_ <: Weight], enableExceptions: Boolean, initialCallGraph: CallGraph) {
      this.solver = solver
      this.precomputedCallGraph = initialCallGraph
    }

    def computeFallback(observableDynamicICFG: ObservableDynamicICFG): Unit = {
      var refined = 0
      var precomputed = 0
      for (s <- Lists.newArrayList(queriedInvokeExpr)) {
        if (!queriedInvokeExprAndAllocationSitesFound.contains(s)) {
          logger.debug("Call graph ends at {}", s)
          precomputed += 1
          if (FALLBACK_OPTION == NoCalleeFoundFallbackOptions.PRECOMPUTED) {
            for (e <- precomputedCallGraph.edgesOutOf(s)) {
              // TODO Refactor. Should not be required, if the backward analysis is sound (data-flow
              // of static fields)
              observableDynamicICFG.addCallIfNotInGraph(e.src(), e.tgt())
            }
          }
          if (FALLBACK_OPTION == NoCalleeFoundFallbackOptions.BYPASS) {
            observableDynamicICFG.notifyNoCalleeFound(s)
          }
        } else {
          refined += 1
        }
      }
      logger.debug("Refined edges {}, fallback to precomputed {}", refined, precomputed)
    }
  
  override def resolveSpecialInvoke(ie: InvokeExpr): Method = {
    val methodFromClassOrFromSuperclass =
      getMethodFromClassOrFromSuperclass(ie.getMethod, ie.getMethod.getDeclaringClass)
    if (methodFromClassOrFromSuperclass.size > 1) {
      throw new RuntimeException(
        "Illegal state, a special call should exactly resolve to one target")
    }
    Iterables.getFirst(methodFromClassOrFromSuperclass, null)
  }

  override def resolveStaticInvoke(ie: InvokeExpr): Method = {
    val methodFromClassOrFromSuperclass =
      getMethodFromClassOrFromSuperclass(ie.getMethod, ie.getMethod.getDeclaringClass)
    if (methodFromClassOrFromSuperclass.size > 1) {
      throw new RuntimeException(
        "Illegal state, a static call should exactly resolve to one target")
    }
    Iterables.getFirst(methodFromClassOrFromSuperclass, null)
  }

  override def resolveInstanceInvoke(stmt: Statement): Collection[Method] = {
    queryForCallees(stmt)
  }

  private def queryForCallees(resolvingStmt: Statement): Collection[Method] = {
    logger.debug("Queried for callees of '{}'.", resolvingStmt)
    // Construct BackwardQuery, so we know which types the object might have
    val invokeExpr = resolvingStmt.getInvokeExpr
    queriedInvokeExpr.add(resolvingStmt)
    val value = invokeExpr.getBase

    val res = new ArrayList[Method]()

    // Not using cfg here because we are iterating backward
    for (pred <- resolvingStmt.getMethod.getControlFlowGraph.getPredsOf(resolvingStmt)) {
      val query = BackwardQuery.make(new Edge(pred, resolvingStmt), value)
      solver.solve(query, false)
      res.addAll(forAnyAllocationSiteOfQuery(query, resolvingStmt, pred))
    }

    res
  }

  @SuppressWarnings(Array("rawtypes"))
  private def forAnyAllocationSiteOfQuery(
      query: BackwardQuery, resolvingStmt: Statement, callSite: Statement): Collection[Method] = {
    val callback = new IterateSolvers(query, callSite, resolvingStmt)
    solver.registerSolverCreationListener(callback)
    callback.results
  }

  private def getMethodFromClassOrFromSuperclass(
      method: DeclaredMethod, sootClass: WrappedClass): Collection[Method] = {
    var res = Sets.newHashSet[Method]()
    var originalClass = sootClass
    while (sootClass != null) {
      for (candidate <- sootClass.getMethods) {
        if (candidate.getSubSignature.equals(method.getSubSignature)) {
          res.add(candidate)
        }
      }
      handlingForThreading(method, sootClass, res)
      if (!res.isEmpty) return res
      if (sootClass.hasSuperclass) {
        sootClass = sootClass.getSuperclass
      } else {
        logDidNotFindMethod(method, originalClass)
        return res
      }
    }
    logDidNotFindMethod(method, originalClass)
    res
  }

  private def logDidNotFindMethod(method: DeclaredMethod, originalClass: WrappedClass): Unit = {
    if (didNotFindMethodLog.put(method, originalClass)) {
      logger.debug("Did not find method {} for class {}", method, originalClass)
    }
  }

  private def handlingForThreading(
      method: DeclaredMethod, sootClass: WrappedClass, res: Set[Method]): Unit = {
    // throw new RuntimeException("Threading not implemented")
    // if (Scene.v().getFastHierarchy().isSubclass(sootClass,
    // Scene.v().getSootClass(THREAD_CLASS)))
    // {
    // if (method.getSignature().equals(THREAD_START_SIGNATURE)) {
    // for (SootMethod candidate : sootClass.getMethods()) {
    // if (candidate.getSubSignature().equals(THREAD_RUN_SUB_SIGNATURE)) {
    // res.add(candidate);
    // }
    // }
    // }
    // }
  }  //

  private final class IterateSolvers[W <: Weight](query: BackwardQuery, unit: Statement, invokeExpr: Statement) extends SolverCreationListener[W] {
    private val results: Collection[Method] = new ArrayList[Method]()

    override def onCreatedSolver(q: Query, solver: AbstractBoomerangSolver[W]): Unit = {
      if (solver.isInstanceOf[ForwardBoomerangSolver[_]]) {
        val forwardQuery = q.asInstanceOf[ForwardQuery]
        val forwardBoomerangSolver = solver.asInstanceOf[ForwardBoomerangSolver[W]]
        for (initialState <- forwardBoomerangSolver.getFieldAutomaton.getInitialStates) {
          forwardBoomerangSolver.getFieldAutomaton.registerListener(
            new ExtractAllocationSiteStateListener[W](initialState, query, forwardQuery) {
              override protected def allocationSiteFound(
                  allocationSite: ForwardQuery, query: BackwardQuery): Unit = {
                logger.debug("Found AllocationSite '{}'.", forwardQuery)
                queriedInvokeExprAndAllocationSitesFound.add(invokeExpr)
                val `type` = forwardQuery.getType
                if (`type`.isRefType) {
                  for (calleeMethod <- getMethodFromClassOrFromSuperclass(
                    invokeExpr.getInvokeExpr.getMethod, `type`.getWrappedClass)) {
                    results.add(calleeMethod)
                  }
                } else if (`type`.isArrayType) {
                  val base = `type`.getArrayBaseType
                  if (base.isRefType) {
                    for (calleeMethod <- getMethodFromClassOrFromSuperclass(
                      invokeExpr.getInvokeExpr.getMethod, base.getWrappedClass)) {
                      results.add(calleeMethod)
                    }
                  }
                }
              }
            }
          )
        }
      }
    }

    override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (query == null) 0 else query.hashCode)
      result = prime * result + (if (invokeExpr == null) 0 else invokeExpr.hashCode)
      result
    }
  
    override def equals(obj: Any): Boolean = {
      if (this == obj) return true
      if (obj == null) return false
      if (getClass != obj.getClass) return false
      val other = obj.asInstanceOf[IterateSolvers[_]]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (query == null) {
        if (other.query != null) return false
      } else if (!query.equals(other.query)) return false
      if (invokeExpr == null) {
        if (other.invokeExpr != null) return false
      } else if (!invokeExpr.equals(other.invokeExpr)) return false
      true
    }

    private def getOuterType: BoomerangResolver = {
      // TODO why is this type of importance?
      BoomerangResolver.this
    }
}
