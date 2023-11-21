package boomerang.callgraph

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.SolverCreationListener
import boomerang.WeightedBoomerang
import boomerang.results.ExtractAllocationSiteStateListener
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.DeclaredMethod
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Type
import boomerang.scene.Val
import boomerang.scene.WrappedClass
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.collect.HashMultimap
import com.google.common.collect.Iterables
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

object BoomerangResolver {
  val FACTORY: Nothing = (solver, cg) => new BoomerangResolver(solver, cg)
  private val logger = LoggerFactory.getLogger(classOf[BoomerangResolver])

  object NoCalleeFoundFallbackOptions extends Enumeration {
    type NoCalleeFoundFallbackOptions = Value
    val PRECOMPUTED, BYPASS = Value
  }

  private val THREAD_CLASS = "java.lang.Thread"
  private val THREAD_START_SIGNATURE = "<java.lang.Thread: void start()>"
  private val THREAD_RUN_SUB_SIGNATURE = "void run()"
  private val FALLBACK_OPTION = NoCalleeFoundFallbackOptions.BYPASS
  private val didNotFindMethodLog = HashMultimap.create
}

class BoomerangResolver extends Nothing {
  private var precomputedCallGraph: Nothing = null
  private var solver: Nothing = null
  private val queriedInvokeExprAndAllocationSitesFound = Sets.newHashSet
  private val queriedInvokeExpr = Sets.newHashSet

  def this(cg: Nothing, scope: Nothing) {
    this()
    this.solver = new Nothing(cg, scope)
    this.precomputedCallGraph = cg
  }

  def this(solver: Nothing, enableExceptions: Boolean, initialCallGraph: Nothing) {
    this()
    this.solver = solver
    this.precomputedCallGraph = initialCallGraph
  }

  def this(solver: Nothing, initialCallGraph: Nothing) {
    this(solver, true, initialCallGraph)
  }

  @Override def computeFallback(observableDynamicICFG: Nothing): Unit = {
    var refined = 0
    var precomputed = 0
    import scala.collection.JavaConversions._
    for (s <- Lists.newArrayList(queriedInvokeExpr)) {
      if (!queriedInvokeExprAndAllocationSitesFound.contains(s)) {
        BoomerangResolver.logger.debug("Call graph ends at {}", s)
        precomputed += 1
        if (BoomerangResolver.FALLBACK_OPTION eq BoomerangResolver.NoCalleeFoundFallbackOptions.PRECOMPUTED) {
          import scala.collection.JavaConversions._
          for (e <- precomputedCallGraph.edgesOutOf(s)) {
            // TODO Refactor. Should not be required, if the backward analysis is sound (data-flow
            // of static fields)
            observableDynamicICFG.addCallIfNotInGraph(e.src, e.tgt)
          }
        }
        if (BoomerangResolver.FALLBACK_OPTION eq BoomerangResolver.NoCalleeFoundFallbackOptions.BYPASS) observableDynamicICFG.notifyNoCalleeFound(s)
      }
      else refined += 1
    }
    BoomerangResolver.logger.debug("Refined edges {}, fallback to precomputed {}", refined, precomputed)
  }

  @Override def resolveSpecialInvoke(ie: Nothing): Nothing = {
    val methodFromClassOrFromSuperclass = getMethodFromClassOrFromSuperclass(ie.getMethod, ie.getMethod.getDeclaringClass)
    if (methodFromClassOrFromSuperclass.size > 1) throw new Nothing("Illegal state, a special call should exactly resolve to one target")
    Iterables.getFirst(methodFromClassOrFromSuperclass, null)
  }

  @Override def resolveStaticInvoke(ie: Nothing): Nothing = {
    val methodFromClassOrFromSuperclass = getMethodFromClassOrFromSuperclass(ie.getMethod, ie.getMethod.getDeclaringClass)
    if (methodFromClassOrFromSuperclass.size > 1) throw new Nothing("Illegal state, a static call should exactly resolve to one target")
    Iterables.getFirst(methodFromClassOrFromSuperclass, null)
  }

  @Override def resolveInstanceInvoke(stmt: Nothing): Nothing = queryForCallees(stmt)

  private def queryForCallees(resolvingStmt: Nothing) = {
    BoomerangResolver.logger.debug("Queried for callees of '{}'.", resolvingStmt)
    // Construct BackwardQuery, so we know which types the object might have
    val invokeExpr = resolvingStmt.getInvokeExpr
    queriedInvokeExpr.add(resolvingStmt)
    val value = invokeExpr.getBase
    val res = new Nothing
    // Not using cfg here because we are iterating backward
    import scala.collection.JavaConversions._
    for (pred <- resolvingStmt.getMethod.getControlFlowGraph.getPredsOf(resolvingStmt)) {
      val query = BackwardQuery.make(new Nothing(pred, resolvingStmt), value)
      solver.solve(query, false)
      res.addAll(forAnyAllocationSiteOfQuery(query, resolvingStmt, pred))
    }
    res
  }

  @SuppressWarnings("rawtypes") private def forAnyAllocationSiteOfQuery(query: Nothing, resolvingStmt: Nothing, callSite: Nothing) = {
    val callback = new BoomerangResolver#IterateSolvers[_ <: Nothing](query, callSite, resolvingStmt)
    solver.registerSolverCreationListener(callback)
    callback.results
  }

  private def getMethodFromClassOrFromSuperclass(method: Nothing, sootClass: Nothing): Nothing = {
    val res = Sets.newHashSet
    val originalClass = sootClass
    while (sootClass != null) {
      import scala.collection.JavaConversions._
      for (candidate <- sootClass.getMethods) {
        if (candidate.getSubSignature.equals(method.getSubSignature)) res.add(candidate)
      }
      handlingForThreading(method, sootClass, res)
      if (!res.isEmpty) return res
      if (sootClass.hasSuperclass) sootClass = sootClass.getSuperclass
      else {
        logDidNotFindMethod(method, originalClass)
        return res
      }
    }
    logDidNotFindMethod(method, originalClass)
    res
  }

  private def logDidNotFindMethod(method: Nothing, originalClass: Nothing): Unit = {
    if (BoomerangResolver.didNotFindMethodLog.put(method, originalClass)) BoomerangResolver.logger.debug("Did not find method {} for class {}", method, originalClass)
  }

  private def handlingForThreading(method: Nothing, sootClass: Nothing, res: Nothing): Unit = {

    // throw new RuntimeException("Threading not implemented");
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
  }

  final private class IterateSolvers[W <: Weight] private(private val query: Nothing, unit: Nothing, private val invokeExpr: Nothing) extends Nothing {
    final private val results = new Nothing

    @Override def onCreatedSolver(q: Nothing, solver: Nothing): Unit = {
      if (solver.isInstanceOf[Nothing]) {
        val forwardQuery = q.asInstanceOf[Nothing]
        val forwardBoomerangSolver = solver.asInstanceOf[Nothing]
        import scala.collection.JavaConversions._
        for (initialState <- forwardBoomerangSolver.getFieldAutomaton.getInitialStates) {
          forwardBoomerangSolver.getFieldAutomaton.registerListener(new Nothing((initialState, query, q.asInstanceOf[Nothing])) {
            @Override protected def allocationSiteFound(allocationSite: Nothing, query: Nothing): Unit = {
              BoomerangResolver.logger.debug("Found AllocationSite '{}'.", forwardQuery)
              queriedInvokeExprAndAllocationSitesFound.add(invokeExpr)
              val `type` = forwardQuery.getType
              if (`type`.isRefType) {
                import scala.collection.JavaConversions._
                for (calleeMethod <- getMethodFromClassOrFromSuperclass(invokeExpr.getInvokeExpr.getMethod, `type`.getWrappedClass)) {
                  results.add(calleeMethod)
                }
              }
              else if (`type`.isArrayType) {
                val base = `type`.getArrayBaseType
                if (base.isRefType) {
                  import scala.collection.JavaConversions._
                  for (calleeMethod <- getMethodFromClassOrFromSuperclass(invokeExpr.getInvokeExpr.getMethod, base.getWrappedClass)) {
                    results.add(calleeMethod)
                  }
                }
              }
            }
          })
        }
      }
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (query == null) 0
      else query.hashCode)
      result = prime * result + (if (invokeExpr == null) 0
      else invokeExpr.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[BoomerangResolver#IterateSolvers[_ <: Nothing]]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (query == null) if (other.query != null) return false
      else if (!query.equals(other.query)) return false
      if (invokeExpr == null) if (other.invokeExpr != null) return false
      else if (!invokeExpr.equals(other.invokeExpr)) return false
      true
    }

    private def getOuterType = {
      // TODO why is this type of importance?
      thisBoomerangResolver
    }
  }
}