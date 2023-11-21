/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang

import boomerang.BoomerangOptions.ArrayStrategy
import boomerang.callgraph.BackwardsObservableICFG
import boomerang.callgraph.ObservableDynamicICFG
import boomerang.callgraph.ObservableICFG
import boomerang.callgraph.ObservableStaticICFG
import boomerang.controlflowgraph.DynamicCFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.controlflowgraph.PredecessorListener
import boomerang.controlflowgraph.StaticCFG
import boomerang.controlflowgraph.SuccessorListener
import boomerang.debugger.Debugger
import boomerang.poi.AbstractPOI
import boomerang.poi.CopyAccessPathChain
import boomerang.poi.ExecuteImportFieldStmtPOI
import boomerang.poi.PointOfIndirection
import boomerang.results.BackwardBoomerangResults
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.AllocVal
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Field
import boomerang.scene.Field.ArrayField
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import boomerang.scene.jimple.JimpleField
import boomerang.scene.jimple.JimpleMethod
import boomerang.scene.jimple.JimpleStaticFieldVal
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.BackwardBoomerangSolver
import boomerang.solver.ControlFlowEdgeBasedFieldTransitionListener
import boomerang.solver.ForwardBoomerangSolver
import boomerang.stats.IBoomerangStats
import boomerang.util.DefaultValueMap
import com.google.common.base.Stopwatch
import com.google.common.collect.HashBasedTable
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import com.google.common.collect.Table
import com.google.common.collect.Table.Cell
import java.util
import java.util.Map.Entry
import java.util.Optional
import java.util.concurrent.TimeUnit
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import soot.SootMethod
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.NodeWithLocation
import sync.pds.solver.nodes.PopNode
import sync.pds.solver.nodes.PushNode
import sync.pds.solver.nodes.SingleNode
import wpds.impl.NestedWeightedPAutomatons
import wpds.impl.Rule
import wpds.impl.SummaryNestedWeightedPAutomatons
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener

object WeightedBoomerang {
  private val LOGGER = LoggerFactory.getLogger(classOf[WeightedBoomerang[_ <: Nothing]])
  private val MAP_PUT_SUB_SIGNATURE = "java.util.Map: java.lang.Object put("
  private val MAP_GET_SUB_SIGNATURE = "java.util.Map: java.lang.Object get("
}

abstract class WeightedBoomerang[W <: Weight](private var callGraph: Nothing, private val dataFlowscope: Nothing, protected val options: Nothing) {
  this.options.checkValid
  this.stats = options.statsFactory
  if (options.onTheFlyControlFlow) this.cfg = new Nothing
  else this.cfg = new Nothing
  if (options.onTheFlyCallGraph) icfg = new Nothing(cfg, options.getResolutionStrategy.newInstance(this, callGraph))
  else icfg = new Nothing(callGraph)
  this.queryGraph = new Nothing(this)
  protected var icfg: Nothing = null
  protected var cfg: Nothing = null
  private val genField = new Nothing
  private var lastTick = 0L
  private var stats: Nothing = null
  private val visitedMethods = Sets.newHashSet
  private val solverCreationListeners = Sets.newHashSet
  private val poiListeners = HashMultimap.create
  private val activatedPoi = HashMultimap.create
  final private val queryToSolvers = new Nothing() {
    @Override protected def createItem(key: Nothing): Nothing = {
      val solver: Nothing = null
      WeightedBoomerang.LOGGER.trace("Forward solving query: {}", key)
      forwardQueries += 1
      solver = createForwardSolver(key)
      stats.registerSolver(key, solver)
      solver.getCallAutomaton.registerListener((t, w, aut) => checkTimeout())
      solver.getFieldAutomaton.registerListener((t, w, aut) => checkTimeout())
      onCreateSubSolver(key, solver)
      solver
    }
  }
  private var forwardQueries = 0
  private val backwardQueries = 0
  final private var queryGraph: Nothing = null
  final private val queryToBackwardSolvers = new Nothing() {
    @Override protected def createItem(key: Nothing): Nothing = {
      if (backwardSolverIns != null) return backwardSolverIns
      val backwardSolver = new Nothing((bwicfg, cfg, genField, key, thisWeightedBoomerang.options, createCallSummaries(null, backwardCallSummaries), createFieldSummaries(null, backwardFieldSummaries), thisWeightedBoomerang.dataFlowscope, options.getBackwardFlowFunction, callGraph.getFieldLoadStatements, callGraph.getFieldStoreStatements, null)) {
        @Override def getFieldWeights: Nothing = thisWeightedBoomerang.getBackwardFieldWeights

        @Override def getCallWeights: Nothing = thisWeightedBoomerang.getBackwardCallWeights

        @Override protected def forceUnbalanced(node: Nothing, sources: Nothing): Boolean = sources.contains(rootQuery) && callAutomaton.isUnbalancedState(node)

        @Override protected def preventCallTransitionAdd(t: Nothing, weight: W): Boolean = {
          checkTimeout()
          super.preventCallTransitionAdd(t, weight)
        }

        @Override protected def preventFieldTransitionAdd(t: Nothing, weight: W): Boolean = {
          checkTimeout()
          super.preventFieldTransitionAdd(t, weight)
        }
      }
      backwardSolver.registerListener((node) => {
        val allocNode = isAllocationNode(node.stmt, node.fact)
        if (allocNode.isPresent || node.stmt.getTarget.isArrayLoad || node.stmt.getTarget.isFieldLoad) backwardSolver.getFieldAutomaton.registerListener(new WeightedBoomerang[W]#EmptyFieldListener(key, node))
        addVisitedMethod(node.stmt.getStart.getMethod)
        handleMapsBackward(node)
        if (options.trackStaticFieldAtEntryPointToClinit) handleStaticInitializer(node, backwardSolver)
      })
      backwardSolverIns = backwardSolver
      backwardSolver
    }
  }

  private def handleStaticInitializer(node: Nothing, backwardSolver: Nothing): Unit = {
    if (options.trackStaticFieldAtEntryPointToClinit && node.fact.isStatic && isFirstStatementOfEntryPoint(node.stmt.getStart)) {
      val fact = node.fact
      if (fact.isInstanceOf[Nothing]) {
        val `val` = fact.asInstanceOf[Nothing]
        import scala.collection.JavaConversions._
        for (m <- `val`.field.asInstanceOf[Nothing].getSootField.getDeclaringClass.getMethods) {
          if (!m.hasActiveBody) continue //todo: continue is not supported
          val jimpleMethod = JimpleMethod.of(m)
          if (m.isStaticInitializer) {
            import scala.collection.JavaConversions._
            for (ep <- icfg.getEndPointsOf(JimpleMethod.of(m))) {
              val newVal = new Nothing(`val`.field.asInstanceOf[Nothing], jimpleMethod)
              cfg.addPredsOfListener(new Nothing(ep) {
                @Override def getPredecessor(pred: Nothing): Unit = {
                  backwardSolver.addNormalCallFlow(node, new Nothing(new Nothing(pred, ep), newVal))
                  backwardSolver.addNormalFieldFlow(node, new Nothing(new Nothing(pred, ep), newVal))
                }
              })
            }
          }
        }
      }
    }
  }

  protected def isFirstStatementOfEntryPoint(stmt: Nothing): Boolean = callGraph.getEntryPoints.contains(stmt.getMethod) && stmt.getMethod.getControlFlowGraph.getStartPoints.contains(stmt)

  protected def handleMapsBackward(node: Nothing): Unit = {
    val rstmt = node.stmt.getStart
    if (rstmt.isAssign && rstmt.containsInvokeExpr && rstmt.getInvokeExpr.toString.contains(WeightedBoomerang.MAP_GET_SUB_SIGNATURE)) if (rstmt.getLeftOp.equals(node.fact)) cfg.addPredsOfListener(new Nothing(rstmt) {
      @Override def getPredecessor(pred: Nothing): Unit = {
        val bwq = BackwardQuery.make(new Nothing(pred, rstmt), rstmt.getInvokeExpr.getArg(0))
        backwardSolve(bwq)
        import scala.collection.JavaConversions._
        for (q <- Lists.newArrayList(queryToSolvers.keySet)) {
          if (queryToSolvers.get(q).getReachedStates.contains(bwq.asNode)) {
            val `var` = q.`var`
            val v = `var`.asInstanceOf[Nothing]
            if (v.getAllocVal.isStringConstant) {
              val key = v.getAllocVal.getStringValue
              backwardSolverIns.propagate(node, new Nothing(new Nothing(pred, rstmt), rstmt.getInvokeExpr.getBase, Field.string(key), PDSSystem.FIELDS))
            }
          }
        }
      }
    })
    if (rstmt.containsInvokeExpr && rstmt.getInvokeExpr.toString.contains(WeightedBoomerang.MAP_PUT_SUB_SIGNATURE)) if (rstmt.getInvokeExpr.getBase.equals(node.fact)) cfg.addPredsOfListener(new Nothing(rstmt) {
      @Override def getPredecessor(pred: Nothing): Unit = {
        val bwq = BackwardQuery.make(new Nothing(pred, rstmt), rstmt.getInvokeExpr.getArg(0))
        backwardSolve(bwq)
        import scala.collection.JavaConversions._
        for (q <- Lists.newArrayList(queryToSolvers.keySet)) {
          if (queryToSolvers.get(q).getReachedStates.contains(bwq.asNode)) {
            val `var` = q.`var`
            val v = `var`.asInstanceOf[Nothing]
            if (v.getAllocVal.isStringConstant) {
              val key = v.getAllocVal.getStringValue
              val succNode = new Nothing(new Nothing(pred, rstmt), rstmt.getInvokeExpr.getArg(1), Field.string(key))
              backwardSolverIns.propagate(node, new Nothing(succNode, PDSSystem.FIELDS))
            }
          }
        }
      }
    })
  }

  protected def handleMapsForward(solver: Nothing, node: Nothing): Unit = {
    val rstmt = node.stmt.getTarget
    if (rstmt.containsInvokeExpr) {
      if (rstmt.isAssign && rstmt.getInvokeExpr.toString.contains(WeightedBoomerang.MAP_GET_SUB_SIGNATURE)) if (rstmt.getInvokeExpr.getBase.equals(node.fact)) {
        val bwq = BackwardQuery.make(node.stmt, rstmt.getInvokeExpr.getArg(0))
        backwardSolve(bwq)
        cfg.addSuccsOfListener(new Nothing(rstmt) {
          @Override def getSuccessor(succ: Nothing): Unit = {
            import scala.collection.JavaConversions._
            for (q <- Lists.newArrayList(queryToSolvers.keySet)) {
              if (queryToSolvers.get(q).getReachedStates.contains(bwq.asNode)) {
                val `var` = q.`var`
                val v = `var`.asInstanceOf[Nothing]
                if (v.getAllocVal.isStringConstant) {
                  val key = v.getAllocVal.getStringValue
                  val succNode = new Nothing(new Nothing(rstmt, succ), rstmt.getLeftOp, Field.string(key))
                  solver.propagate(node, new Nothing(succNode, PDSSystem.FIELDS))
                }
              }
            }
          }
        })
      }
      if (rstmt.getInvokeExpr.toString.contains(WeightedBoomerang.MAP_PUT_SUB_SIGNATURE)) if (rstmt.getInvokeExpr.getArg(1).equals(node.fact)) {
        val bwq = BackwardQuery.make(node.stmt, rstmt.getInvokeExpr.getArg(0))
        backwardSolve(bwq)
        cfg.addSuccsOfListener(new Nothing(rstmt) {
          @Override def getSuccessor(succ: Nothing): Unit = {
            import scala.collection.JavaConversions._
            for (q <- Lists.newArrayList(queryToSolvers.keySet)) {
              if (queryToSolvers.get(q).getReachedStates.contains(bwq.asNode)) {
                val `var` = q.`var`
                val v = `var`.asInstanceOf[Nothing]
                if (v.getAllocVal.isStringConstant) {
                  val key = v.getAllocVal.getStringValue
                  solver.propagate(node, new Nothing(new Nothing(rstmt, succ), rstmt.getInvokeExpr.getBase, Field.string(key), PDSSystem.FIELDS))
                }
              }
            }
          }
        })
      }
    }
  }

  private var backwardSolverIns: Nothing = null
  private var solving = false

  def checkTimeout(): Unit = {
    if (options.analysisTimeoutMS > 0) {
      val elapsed = analysisWatch.elapsed(TimeUnit.MILLISECONDS)
      if (elapsed - lastTick > 15000) {
        WeightedBoomerang.LOGGER.debug("Elapsed Time: {}/{}, Visited Methods {}", elapsed, options.analysisTimeoutMS, visitedMethods.size)
        WeightedBoomerang.LOGGER.debug("Forward / Backward Queries: {}/{}", forwardQueries, backwardQueries)
        if (WeightedBoomerang.LOGGER.isDebugEnabled) {
          printElapsedTimes()
          printRules()
          printStats()
        }
        lastTick = elapsed
      }
      if (options.analysisTimeoutMS < elapsed) {
        if (analysisWatch.isRunning) analysisWatch.stop
        throw new Nothing(elapsed, stats)
      }
    }
  }

  private var bwicfg: Nothing = null
  private val backwardCallSummaries = new Nothing
  private val backwardFieldSummaries = new Nothing
  private val forwardCallSummaries = new Nothing
  private val forwardFieldSummaries = new Nothing
  private val fieldWrites = new Nothing() {
    @Override protected def createItem(key: WeightedBoomerang[W]#FieldWritePOI): WeightedBoomerang[W]#FieldWritePOI = {
      stats.registerFieldWritePOI(key)
      key
    }
  }
  private val analysisWatch = Stopwatch.createUnstarted
  private var rootQuery: Nothing = null

  def this(cg: Nothing, scope: Nothing) {
    this(cg, scope, new Nothing)
  }

  protected def addVisitedMethod(method: Nothing): Unit = {
    if (!dataFlowscope.isExcluded(method) && visitedMethods.add(method)) WeightedBoomerang.LOGGER.trace("Reach Method: {}", method)
  }

  protected def isAllocationNode(s: Nothing, fact: Nothing): Nothing = options.getAllocationVal(s.getStart.getMethod, s.getStart, fact)

  protected def createForwardSolver(sourceQuery: Nothing): Nothing = {
    val solver = new Nothing((icfg, cfg, sourceQuery, genField, options, createCallSummaries(sourceQuery, forwardCallSummaries), createFieldSummaries(sourceQuery, forwardFieldSummaries), dataFlowscope, options.getForwardFlowFunctions, callGraph.getFieldLoadStatements, callGraph.getFieldStoreStatements, sourceQuery.getType)) {
      @Override def getCallWeights: Nothing = thisWeightedBoomerang.getForwardCallWeights(sourceQuery)

      @Override def getFieldWeights: Nothing = thisWeightedBoomerang.getForwardFieldWeights

      @Override def addCallRule(rule: Nothing): Unit = {
        if (preventCallRuleAdd(sourceQuery, rule)) return
        super.addCallRule(rule)
      }

      @Override protected def forceUnbalanced(node: Nothing, sources: Nothing): Boolean = queryGraph.isRoot(sourceQuery)

      @Override protected def overwriteFieldAtStatement(fieldWriteStatement: Nothing, killedTransition: Nothing): Unit = {
        val backwardQuery = BackwardQuery.make(killedTransition.getTarget.fact.stmt, fieldWriteStatement.getTarget.getRightOp)
        val copyAccessPathChain = new Nothing(queryToSolvers.get(sourceQuery), queryToBackwardSolvers.getOrCreate(backwardQuery), fieldWriteStatement, killedTransition)
        copyAccessPathChain.exec
        queryGraph.addEdge(sourceQuery, killedTransition.getStart.fact, backwardQuery)
      }

      @Override protected def preventCallTransitionAdd(t: Nothing, weight: W): Boolean = {
        checkTimeout()
        super.preventCallTransitionAdd(t, weight)
      }

      @Override protected def preventFieldTransitionAdd(t: Nothing, weight: W): Boolean = {
        checkTimeout()
        super.preventFieldTransitionAdd(t, weight)
      }
    }
    solver.registerListener((node) => {
      if (node.stmt.getStart.isFieldStore) forwardHandleFieldWrite(node, createFieldStore(node.stmt), sourceQuery)
      else if ((options.getArrayStrategy ne ArrayStrategy.DISABLED) && node.stmt.getStart.isArrayStore) forwardHandleFieldWrite(node, createArrayFieldStore(node.stmt), sourceQuery)
      addVisitedMethod(node.stmt.getStart.getMethod)
      handleMapsForward(solver, node)
    })
    solver
  }

  private def createCallSummaries(sourceQuery: Nothing, summaries: Nothing) = new Nothing() {
    @Override def putSummaryAutomaton(target: Nothing, aut: Nothing): Unit = {
      summaries.putSummaryAutomaton(target, aut)
    }

    @Override def getSummaryAutomaton(target: Nothing): Nothing = {
      if (sourceQuery.`var`.isInstanceOf[Nothing]) {
        val allocVal = sourceQuery.`var`.asInstanceOf[Nothing]
        var f: Nothing = null
        if (target.fact.isUnbalanced) f = target.fact.asUnbalanced(null)
        else f = target.fact
        if (f.equals(allocVal.getDelegate)) return queryToSolvers.getOrCreate(sourceQuery).getCallAutomaton
      }
      summaries.getSummaryAutomaton(target)
    }
  }

  private def createFieldSummaries(sourceQuery: Nothing, summaries: Nothing) = new Nothing() {
    @Override def putSummaryAutomaton(target: Nothing, aut: Nothing): Unit = {
      summaries.putSummaryAutomaton(target, aut)
    }

    @Override def getSummaryAutomaton(target: Nothing): Nothing = {
      if (target.fact.equals(sourceQuery.asNode)) return queryToSolvers.getOrCreate(sourceQuery).getFieldAutomaton
      summaries.getSummaryAutomaton(target)
    }
  }

  def preventCallRuleAdd(sourceQuery: Nothing, rule: Nothing) = false

  protected def createArrayFieldStore(s: Nothing): WeightedBoomerang[W]#FieldWritePOI = {
    val base = s.getStart.getArrayBase
    fieldWrites.getOrCreate(new WeightedBoomerang[W]#FieldWritePOI(s, base.getX, Field.array(base.getY), s.getStart.getRightOp))
  }

  protected def createFieldStore(cfgEdge: Nothing): WeightedBoomerang[W]#FieldWritePOI = {
    val s = cfgEdge.getStart
    val base = s.getFieldStore.getX
    val field = s.getFieldStore.getY
    val stored = s.getRightOp
    fieldWrites.getOrCreate(new WeightedBoomerang[W]#FieldWritePOI(cfgEdge, base, field, stored))
  }

  protected def forwardHandleFieldWrite(node: Nothing, fieldWritePoi: WeightedBoomerang[W]#FieldWritePOI, sourceQuery: Nothing): Unit = {
    val backwardQuery = BackwardQuery.make(node.stmt, fieldWritePoi.getBaseVar)
    if (node.fact.equals(fieldWritePoi.getStoredVar)) {
      backwardSolve(backwardQuery)
      queryGraph.addEdge(sourceQuery, node, backwardQuery)
      queryToSolvers.get(sourceQuery).registerStatementFieldTransitionListener(new WeightedBoomerang[W]#ForwardHandleFieldWrite(sourceQuery, fieldWritePoi, node.stmt))
    }
    if (node.fact.equals(fieldWritePoi.getBaseVar)) queryToSolvers.getOrCreate(sourceQuery).getFieldAutomaton.registerListener(new WeightedBoomerang[W]#TriggerBaseAllocationAtFieldWrite(new Nothing(node), fieldWritePoi, sourceQuery))
  }

  def unregisterAllListeners(): Unit = {
    import scala.collection.JavaConversions._
    for (solver <- queryToSolvers.values) {
      solver.unregisterAllListeners
    }
    import scala.collection.JavaConversions._
    for (solver <- queryToBackwardSolvers.values) {
      solver.unregisterAllListeners
    }
    this.cfg.unregisterAllListeners
    this.queryGraph.unregisterAllListeners
    this.poiListeners.clear
    this.activatedPoi.clear
    this.fieldWrites.clear
  }

  def getBackwardSolvers: Nothing = queryToBackwardSolvers

  def getQueryGraph: Nothing = queryGraph

  final private class EmptyFieldListener(private var key: Nothing, private var node: Nothing) extends Nothing(new Nothing(node)) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getEnclosingInstance.hashCode
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedBoomerang[W]#EmptyFieldListener]
      if (!getEnclosingInstance.equals(other.getEnclosingInstance)) return false
      true
    }

    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (!t.getLabel.equals(Field.empty) && !t.getLabel.isInstanceOf[Nothing]) return
      val allocNode = isAllocationNode(node.stmt, node.fact)
      if (allocNode.isPresent) {
        val `val` = allocNode.get
        var forwardQuery: Nothing = null
        if (t.getLabel.isInstanceOf[Nothing]) thisWeightedBoomerang.backwardSolverIns.getFieldAutomaton.registerListener(new WeightedBoomerang[W]#ArrayAllocationListener(t.getLabel.asInstanceOf[Nothing].getIndex, t.getTarget, `val`, key, node))
        else {
          forwardQuery = new Nothing(node.stmt, `val`)
          forwardSolve(forwardQuery)
          queryGraph.addEdge(key, node, forwardQuery)
        }
      }
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    private def getEnclosingInstance = thisWeightedBoomerang
  }

  final private class ArrayAllocationListener(private val arrayAccessIndex: Int, target: Nothing, private var `val`: Nothing, private var key: Nothing, private var node: Nothing) extends Nothing(target) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(Field.empty)) {
        val forwardQuery = new Nothing(node.stmt, `val`, arrayAccessIndex)
        forwardSolve(forwardQuery)
        queryGraph.addEdge(key, node, forwardQuery)
      }
      if (t.getLabel.isInstanceOf[Nothing]) {
        val forwardQuery = new Nothing(node.stmt, `val`, arrayAccessIndex, t.getLabel.asInstanceOf[Nothing].getIndex)
        forwardSolve(forwardQuery)
        queryGraph.addEdge(key, node, forwardQuery)
      }
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    private def getEnclosingInstance = thisWeightedBoomerang

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getEnclosingInstance.hashCode
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedBoomerang[W]#ArrayAllocationListener]
      if (!getEnclosingInstance.equals(other.getEnclosingInstance)) return false
      true
    }
  }

  final private class ForwardHandleFieldWrite private(private val sourceQuery: Nothing, private val fieldWritePoi: Nothing, private val stmt: Nothing) extends Nothing(statement) {
    @Override def onAddedTransition(t: Nothing): Unit = {
      if (t.getStart.isInstanceOf[Nothing]) return
      if (t.getStart.fact.stmt.equals(stmt)) fieldWritePoi.addFlowAllocation(sourceQuery)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (sourceQuery == null) 0
      else sourceQuery.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedBoomerang[W]#ForwardHandleFieldWrite]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (sourceQuery == null) if (other.sourceQuery != null) return false
      else if (!sourceQuery.equals(other.sourceQuery)) return false
      true
    }

    private def getOuterType = thisWeightedBoomerang
  }

  private class TriggerBaseAllocationAtFieldWrite(state: Nothing, private val fieldWritePoi: Nothing, private val sourceQuery: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      if (isAllocationNode(t.getTarget.fact.fact, sourceQuery)) fieldWritePoi.addBaseAllocation(sourceQuery)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (fieldWritePoi == null) 0
      else fieldWritePoi.hashCode)
      result = prime * result + (if (sourceQuery == null) 0
      else sourceQuery.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedBoomerang[W]#TriggerBaseAllocationAtFieldWrite]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (fieldWritePoi == null) if (other.fieldWritePoi != null) return false
      else if (!fieldWritePoi.equals(other.fieldWritePoi)) return false
      if (sourceQuery == null) if (other.sourceQuery != null) return false
      else if (!sourceQuery.equals(other.sourceQuery)) return false
      true
    }

    private def getOuterType = thisWeightedBoomerang
  }

  private def isAllocationNode(fact: Nothing, sourceQuery: Nothing) = {
    /*TODO Replace by new designated type: see AbstractBoomerangSolver*/
    fact.equals(sourceQuery.`var`.asUnbalanced(sourceQuery.cfgEdge))
  }

  private def bwicfg = {
    if (bwicfg == null) bwicfg = new Nothing(icfg)
    bwicfg
  }

  def solve(query: Nothing): Nothing = {
    if (!options.allowMultipleQueries && solving) throw new Nothing("One cannot re-use the same Boomerang solver for more than one query, unless option allowMultipleQueries is enabled. If allowMultipleQueries is enabled, ensure to call unregisterAllListeners() on this instance upon termination of all queries.")
    solving = true
    if (!analysisWatch.isRunning) analysisWatch.start
    var timedout = false
    try {
      queryGraph.addRoot(query)
      WeightedBoomerang.LOGGER.trace("Starting forward analysis of: {}", query)
      forwardSolve(query)
      WeightedBoomerang.LOGGER.trace("Query terminated in {} ({}), visited methods {}", analysisWatch, query, visitedMethods.size)
      WeightedBoomerang.LOGGER.trace("Query Graph \n{}", queryGraph.toDotString)
      icfg.computeFallback
    } catch {
      case e: Nothing =>
        timedout = true
        WeightedBoomerang.LOGGER.trace("Timeout ({}) of query: {}, visited methods {}", analysisWatch, query, visitedMethods.size)
      case e: Nothing =>
        WeightedBoomerang.LOGGER.error("Solving query crashed in {}", e)
    }
    if (!options.allowMultipleQueries) unregisterAllListeners()
    if (analysisWatch.isRunning) analysisWatch.stop
    new Nothing(query, icfg, cfg, timedout, this.queryToSolvers, getStats, analysisWatch, visitedMethods, options.trackDataFlowPath, options.prunePathConditions, options.trackImplicitFlows)
  }

  def solve(query: Nothing): Nothing = solve(query, true)

  def solve(query: Nothing, timing: Boolean): Nothing = {
    if (!options.allowMultipleQueries && solving) throw new Nothing("One cannot re-use the same Boomerang solver for more than one query, unless option allowMultipleQueries is enabled. If allowMultipleQueries is enabled, ensure to call unregisterAllListeners() on this instance upon termination of all queries.")
    solving = true
    if (timing && !analysisWatch.isRunning) analysisWatch.start
    var timedout = false
    try {
      queryGraph.addRoot(query)
      WeightedBoomerang.LOGGER.trace("Starting backward analysis of: {}", query)
      backwardSolve(query)
    } catch {
      case e: Nothing =>
        timedout = true
        WeightedBoomerang.LOGGER.trace("Timeout ({}) of query: {} ", analysisWatch, query)
    }
    debugOutput()
    // printAllBackwardCallAutomatonFlow();
    if (!options.allowMultipleQueries) unregisterAllListeners()
    if (timing && analysisWatch.isRunning) analysisWatch.stop
    new Nothing(query, timedout, this.queryToSolvers, backwardSolverIns, getStats, analysisWatch)
  }

  def solveUnderScope(query: Nothing, triggeringNode: Nothing, parentQuery: Nothing): Nothing = {
    if (!options.allowMultipleQueries && solving) throw new Nothing("One cannot re-use the same Boomerang solver for more than one query, unless option allowMultipleQueries is enabled. If allowMultipleQueries is enabled, ensure to call unregisterAllListeners() on this instance upon termination of all queries.")
    solving = true
    if (!analysisWatch.isRunning) analysisWatch.start
    var timedout = false
    try {
      WeightedBoomerang.LOGGER.trace("Starting backward analysis of: {}", query)
      backwardSolve(query)
      queryGraph.addEdge(parentQuery, triggeringNode, query)
      this.debugOutput()
    } catch {
      case e: Nothing =>
        timedout = true
        WeightedBoomerang.LOGGER.trace("Timeout ({}) of query: {} ", analysisWatch, query)
    }
    if (analysisWatch.isRunning) analysisWatch.stop
    new Nothing(query, timedout, this.queryToSolvers, backwardSolverIns, getStats, analysisWatch)
  }

  def solveUnderScope(query: Nothing, triggeringNode: Nothing, parentQuery: Nothing): Nothing = {
    if (!options.allowMultipleQueries && solving) throw new Nothing("One cannot re-use the same Boomerang solver for more than one query, unless option allowMultipleQueries is enabled. If allowMultipleQueries is enabled, ensure to call unregisterAllListeners() on this instance upon termination of all queries.")
    solving = true
    if (!analysisWatch.isRunning) analysisWatch.start
    var timedout = false
    try {
      WeightedBoomerang.LOGGER.trace("Starting forward analysis of: {}", query)
      forwardSolve(query)
      queryGraph.addEdge(parentQuery, triggeringNode, query)
      WeightedBoomerang.LOGGER.trace("Query terminated in {} ({}), visited methods {}", analysisWatch, query, visitedMethods.size)
    } catch {
      case e: Nothing =>
        timedout = true
        WeightedBoomerang.LOGGER.trace("Timeout ({}) of query: {}, visited methods {}", analysisWatch, query, visitedMethods.size)
      case e: Nothing =>
        WeightedBoomerang.LOGGER.error("Solving query crashed in {}", e)
    }
    if (!options.allowMultipleQueries) unregisterAllListeners()
    if (analysisWatch.isRunning) analysisWatch.stop
    new Nothing(query, icfg, cfg, timedout, this.queryToSolvers, getStats, analysisWatch, visitedMethods, options.trackDataFlowPath, options.prunePathConditions, options.trackImplicitFlows)
  }

  def debugOutput(): Unit = {
    if (WeightedBoomerang.LOGGER.isTraceEnabled) {
      WeightedBoomerang.LOGGER.trace("Query Graph \n{}", queryGraph.toDotString)
      // LOGGER.trace("Terminated backward analysis of: {}", query);
      WeightedBoomerang.LOGGER.trace("#ForwardSolvers: {}", queryToSolvers.size)
      printAllAutomata()
      printAllForwardCallAutomatonFlow()
      printAllBackwardCallAutomatonFlow()
    }
  }

  protected def backwardSolve(query: Nothing): Unit = {
    if (!options.aliasing) return
    val solver = queryToBackwardSolvers.getOrCreate(query)
    val fieldTarget = solver.createQueryNodeField(query)
    val callTarget = solver.generateCallState(new Nothing(query.`var`), query.cfgEdge)
    if (rootQuery == null) rootQuery = callTarget
    solver.solve(query.asNode, Field.empty, fieldTarget, query.cfgEdge, callTarget)
  }

  private def forwardSolve(query: Nothing) = {
    val cfgEdge = query.asNode.stmt
    val solver = queryToSolvers.getOrCreate(query)
    val fieldTarget = solver.createQueryNodeField(query)
    val callTarget = solver.generateCallState(new Nothing(query.`var`), query.cfgEdge)
    val stmt = cfgEdge.getStart
    if (!stmt.isFieldStore && query.isInstanceOf[Nothing] && (options.getArrayStrategy ne ArrayStrategy.DISABLED)) if (query.isInstanceOf[Nothing]) {
      val arrayQuery = query.asInstanceOf[Nothing]
      val node = new Nothing(query.cfgEdge, query.`var`.asInstanceOf[Nothing].getDelegate)
      val sourveVal = new Nothing(node)
      val genState1 = solver.generateFieldState(sourveVal, Field.array(arrayQuery.getIndex1))
      insertTransition(solver.getFieldAutomaton, new Nothing(sourveVal, Field.array(arrayQuery.getIndex1), genState1))
      val genState2 = solver.generateFieldState(sourveVal, Field.array(arrayQuery.getIndex2))
      insertTransition(solver.getFieldAutomaton, new Nothing(genState1, Field.array(arrayQuery.getIndex2), genState2))
      insertTransition(solver.getFieldAutomaton, new Nothing(genState2, Field.empty, fieldTarget))
    }
    else {
      val arrayQuery = query.asInstanceOf[Nothing]
      val node = new Nothing(query.cfgEdge, query.`var`.asInstanceOf[Nothing].getDelegate)
      val sourceVal = new Nothing(node)
      val genState = solver.generateFieldState(sourceVal, Field.array(arrayQuery.getIndex))
      insertTransition(solver.getFieldAutomaton, new Nothing(sourceVal, Field.array(arrayQuery.getIndex), genState))
      insertTransition(solver.getFieldAutomaton, new Nothing(genState, Field.empty, fieldTarget))
    }
    if (stmt.isStringAllocation) {
    }
    var `var`: Nothing = null
    var field: Nothing = null
    if (stmt.isFieldStore) {
      field = stmt.getFieldStore.getY
      `var` = stmt.getFieldStore.getX
      forwardHandleFieldWrite(new Nothing(cfgEdge, stmt.getRightOp), new WeightedBoomerang[W]#FieldWritePOI(cfgEdge, `var`, field, stmt.getRightOp), query)
    }
    else {
      `var` = query.`var`.asInstanceOf[Nothing].getDelegate
      field = Field.empty
    }
    if (query.isInstanceOf[Nothing]) {
      val q = query.asInstanceOf[Nothing]
      // Convert AllocVal -> Val
      solver.solve(new Nothing(cfgEdge, `var`), field, fieldTarget, cfgEdge, callTarget, q.weight)
    }
    else {
      // Convert AllocVal -> Val
      solver.solve(new Nothing(cfgEdge, `var`), field, fieldTarget, cfgEdge, callTarget)
    }
    solver
  }

  private def insertTransition(aut: Nothing, transition: Nothing): Boolean = {
    if (!aut.nested) return aut.addTransition(transition)
    val target = transition.getTarget
    if (!target.isInstanceOf[Nothing]) {
      forwardFieldSummaries.putSummaryAutomaton(target, aut)
      aut.registerListener((t, w, aut12) => {
        if (t.getStart.isInstanceOf[Nothing]) {
          val n = forwardFieldSummaries.getSummaryAutomaton(t.getStart)
          aut12.addNestedAutomaton(n)
        }
      })
      return aut.addTransition(transition)
    }
    val nested = forwardFieldSummaries.getSummaryAutomaton(target)
    nested.registerListener((t, w, aut1) => {
      if (t.getStart.isInstanceOf[Nothing]) {
        val n = forwardFieldSummaries.getSummaryAutomaton(t.getStart)
        aut1.addNestedAutomaton(n)
      }
    })
    nested.addTransition(transition)
  }

  class FieldWritePOI(statement: Nothing, base: Nothing, field: Nothing, stored: Nothing) extends Nothing(statement, base, field, stored) {
    @Override def execute(baseAllocation: Nothing, flowAllocation: Nothing): Unit = {
      if (flowAllocation.isInstanceOf[Nothing]) {
      }
      else if (flowAllocation.isInstanceOf[Nothing]) {
        val baseSolver = queryToSolvers.get(baseAllocation)
        val flowSolver = queryToSolvers.get(flowAllocation)
        val exec = new Nothing((baseSolver, flowSolver, thisFieldWritePOI)) {
          def activate(start: Nothing): Unit = {
            activateAllPois(new WeightedBoomerang[W]#SolverPair(flowSolver, baseSolver), start)
          }
        }
        registerActivationListener(new WeightedBoomerang[W]#SolverPair(flowSolver, baseSolver), exec)
        exec.solve
      }
    }
  }

  protected def activateAllPois(pair: WeightedBoomerang[W]#SolverPair, start: Nothing): Unit = {
    if (activatedPoi.put(pair, start)) {
      val listeners = poiListeners.get(pair)
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(listeners)) {
        l.trigger(start)
      }
    }
  }

  def registerActivationListener(solverPair: WeightedBoomerang[W]#SolverPair, exec: Nothing): Unit = {
    val listeners = activatedPoi.get(solverPair)
    import scala.collection.JavaConversions._
    for (node <- Lists.newArrayList(listeners)) {
      exec.trigger(node)
    }
    poiListeners.put(solverPair, exec)
  }

  private class SolverPair(private var flowSolver: Nothing, private var baseSolver: Nothing) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (baseSolver == null) 0
      else baseSolver.hashCode)
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedBoomerang[W]#SolverPair]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (baseSolver == null) if (other.baseSolver != null) return false
      else if (!baseSolver.equals(other.baseSolver)) return false
      if (flowSolver == null) if (other.flowSolver != null) return false
      else if (!flowSolver.equals(other.flowSolver)) return false
      true
    }

    private def getOuterType = thisWeightedBoomerang
  }

  def icfg: Nothing = icfg

  def cfg: Nothing = cfg

  protected def getForwardFieldWeights: Nothing

  protected def getBackwardFieldWeights: Nothing

  protected def getBackwardCallWeights: Nothing

  protected def getForwardCallWeights(sourceQuery: Nothing): Nothing

  def getSolvers: Nothing = queryToSolvers

  def debugOutput(debugger: Nothing): Unit = {
    debugger.done(icfg, cfg, visitedMethods, queryToSolvers)
  }

  def getStats: Nothing = stats

  def onCreateSubSolver(key: Nothing, solver: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (l <- solverCreationListeners) {
      l.onCreatedSolver(key, solver)
    }
  }

  def registerSolverCreationListener(l: Nothing): Unit = {
    if (solverCreationListeners.add(l)) {
      import scala.collection.JavaConversions._
      for (e <- Lists.newArrayList(queryToSolvers.entrySet)) {
        l.onCreatedSolver(e.getKey, e.getValue)
      }
    }
  }

  def getResults(seed: Nothing): Nothing = {
    val results = HashBasedTable.create
    val fieldAut = queryToSolvers.getOrCreate(seed).getCallAutomaton
    import scala.collection.JavaConversions._
    for (e <- fieldAut.getTransitionsToFinalWeights.entrySet) {
      val t = e.getKey
      val w = e.getValue
      if (t.getLabel.equals(Statement.epsilon)) continue //todo: continue is not supported
      if (t.getStart.fact.isLocal && !t.getLabel.getStart.getMethod.equals(t.getStart.fact.m)) continue //todo: continue is not supported
      results.put(t.getLabel, t.getStart.fact, w)
    }
    results
  }

  def getOptions: Nothing = this.options

  def getCallGraph: Nothing = this.callGraph

  def getVisitedMethods: Nothing = visitedMethods

  // For debugging purpose
  def printCallAutomatonFlow(solver: Nothing): Unit = {
    val results = HashBasedTable.create
    val callAut = solver.getCallAutomaton
    import scala.collection.JavaConversions._
    for (e <- callAut.getTransitionsToFinalWeights.entrySet) {
      val t = e.getKey
      val w = e.getValue
      if (t.getLabel.getStart.equals(Statement.epsilon)) continue //todo: continue is not supported
      if (t.getStart.fact.isLocal && !t.getLabel.getStart.getMethod.equals(t.getStart.fact.m)) continue //todo: continue is not supported
      results.put(t.getLabel, t.getStart.fact, w)
    }
    WeightedBoomerang.LOGGER.trace("Call Automaton flow for {}", solver)
    printResultsPerMethod(results)
  }

  private def printResultsPerMethod(results: Nothing): Unit = {
    val methodToRes = HashMultimap.create
    import scala.collection.JavaConversions._
    for (c <- results.cellSet) {
      methodToRes.put(c.getRowKey.getStart.getMethod, c)
    }
    import scala.collection.JavaConversions._
    for (m <- methodToRes.keySet) {
      WeightedBoomerang.LOGGER.trace("Results in Method {}: ", m)
      import scala.collection.JavaConversions._
      for (s <- m.getStatements) {
        WeightedBoomerang.LOGGER.trace("\tStatement {}: ", s)
        import scala.collection.JavaConversions._
        for (c <- methodToRes.get(m)) {
          if (c.getRowKey.getStart.equals(s)) WeightedBoomerang.LOGGER.trace("\t\tVal: {}, W: {}", c.getColumnKey, c.getValue + " ")
        }
      }
    }
  }

  def printAllForwardCallAutomatonFlow(): Unit = {
    import scala.collection.JavaConversions._
    for (e <- queryToSolvers.entrySet) {
      printCallAutomatonFlow(e.getValue)
    }
  }

  def printAllBackwardCallAutomatonFlow(): Unit = {
    import scala.collection.JavaConversions._
    for (e <- queryToBackwardSolvers.entrySet) {
      printCallAutomatonFlow(e.getValue)
    }
  }

  def printAllAutomata(): Unit = {
    import scala.collection.JavaConversions._
    for (e <- queryToSolvers.entrySet) {
      printAutomata(e.getKey)
    }
    WeightedBoomerang.LOGGER.trace("Backward Solver")
    import scala.collection.JavaConversions._
    for (e <- queryToBackwardSolvers.entrySet) {
      printAutomata(e.getKey)
    }
  }

  def printAutomata(q: Nothing): Unit = {
    if (WeightedBoomerang.LOGGER.isTraceEnabled && q.isInstanceOf[Nothing]) {
      WeightedBoomerang.LOGGER.trace("Solver {}", queryToSolvers.get(q).getQuery)
      WeightedBoomerang.LOGGER.trace("Field Automaton\n {}", queryToSolvers.get(q).getFieldAutomaton.toDotString)
      WeightedBoomerang.LOGGER.trace("Call Automaton\n {}", queryToSolvers.get(q).getCallAutomaton.toDotString)
    }
    if (WeightedBoomerang.LOGGER.isTraceEnabled && q.isInstanceOf[Nothing]) {
      WeightedBoomerang.LOGGER.trace("Solver {}", queryToBackwardSolvers.get(q))
      WeightedBoomerang.LOGGER.trace("Field Automaton\n {}", queryToBackwardSolvers.get(q).getFieldAutomaton.toDotString)
      WeightedBoomerang.LOGGER.trace("Call Automaton\n {}", queryToBackwardSolvers.get(q).getCallAutomaton.toDotString)
    }
  }

  private def printStats(): Unit = {
    var forwardCallTransitions = 0
    var backwardCallTransitions = 0
    var forwardFieldTransitions = 0
    var backwardFieldTransitions = 0
    import scala.collection.JavaConversions._
    for (e <- queryToSolvers.entrySet) {
      if (e.getKey.isInstanceOf[Nothing]) {
        forwardCallTransitions += e.getValue.getCallAutomaton.getTransitions.size
        forwardFieldTransitions += e.getValue.getFieldAutomaton.getTransitions.size
      }
      else {
        backwardCallTransitions += e.getValue.getCallAutomaton.getTransitions.size
        backwardFieldTransitions += e.getValue.getFieldAutomaton.getTransitions.size
      }
    }
    WeightedBoomerang.LOGGER.trace("Forward Call Transitions: {}", forwardCallTransitions)
    WeightedBoomerang.LOGGER.trace("Forward Field Transitions: {}", forwardFieldTransitions)
    WeightedBoomerang.LOGGER.trace("Backward Call Transitions: {}", backwardCallTransitions)
    WeightedBoomerang.LOGGER.trace("Backward Field Transitions: {}", backwardFieldTransitions)
  }

  private def printQueryTimes(): Unit = {
    import scala.collection.JavaConversions._
    for (entry <- queryToSolvers.entrySet) {
      WeightedBoomerang.LOGGER.trace("{}", entry.getKey)
      WeightedBoomerang.LOGGER.trace("Call Automaton Time: {}", entry.getValue.getCallAutomaton.getWatch)
      WeightedBoomerang.LOGGER.trace("Field Automaton Time: {}", entry.getValue.getFieldAutomaton.getWatch)
    }
  }

  private def printElapsedTimes(): Unit = {
    // LOGGER.debug("BackwardCallWatch " + backwardSolver.getCallAutomaton().getWatch());
    // LOGGER.debug("BackwardFieldWatch " + backwardSolver.getFieldAutomaton().getWatch());
    var forwardCallElaps = 0
    var forwardFieldElaps = 0
    import scala.collection.JavaConversions._
    for (v <- queryToSolvers.values) {
      forwardCallElaps += v.getCallAutomaton.getWatch.elapsed(TimeUnit.SECONDS)
      forwardFieldElaps += v.getFieldAutomaton.getWatch.elapsed(TimeUnit.SECONDS)
    }
    WeightedBoomerang.LOGGER.trace("ForwardCallWatch " + forwardCallElaps + " s")
    WeightedBoomerang.LOGGER.trace("ForwardFieldWatch " + forwardFieldElaps + " s")
  }

  private def printRules(): Unit = {
    // LOGGER.debug("BackwardCallRules " + backwardSolver.getCallPDS().getAllRules().size());
    // LOGGER.debug("BackwardFieldRules " + backwardSolver.getFieldPDS().getAllRules().size());
    var forwardCallElaps = 0
    var forwardFieldElaps = 0
    val allCallRules = Sets.newHashSet
    val allFieldRules = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (v <- queryToSolvers.values) {
      allCallRules.addAll(v.getCallPDS.getAllRules)
      allFieldRules.addAll(v.getFieldPDS.getAllRules)
      forwardCallElaps += v.getCallPDS.getAllRules.size
      forwardFieldElaps += v.getFieldPDS.getAllRules.size
    }
    WeightedBoomerang.LOGGER.trace("ForwardCallRules (total)" + forwardCallElaps)
    WeightedBoomerang.LOGGER.trace("ForwardFieldRules (total)" + forwardFieldElaps)
    WeightedBoomerang.LOGGER.trace("ForwardCallRules (deduplicated)" + allCallRules.size)
    WeightedBoomerang.LOGGER.trace("ForwardFieldRules (deduplicated)" + allFieldRules.size)
  }
}