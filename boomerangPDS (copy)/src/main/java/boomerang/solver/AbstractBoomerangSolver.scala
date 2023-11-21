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
package boomerang.solver

import boomerang.BoomerangOptions
import boomerang.Query
import boomerang.callgraph.BackwardsObservableICFG
import boomerang.callgraph.CallerListener
import boomerang.callgraph.ObservableICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Field
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Type
import boomerang.scene.Val
import boomerang.util.RegExAccessPath
import com.google.common.base.Stopwatch
import com.google.common.collect.HashBasedTable
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import com.google.common.collect.Table
import java.util
import java.util.Map.Entry
import org.slf4j.LoggerFactory
import pathexpression.IRegEx
import sync.pds.solver.EmptyStackWitnessListener
import sync.pds.solver.SyncPDSSolver
import sync.pds.solver.WitnessListener
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import wpds.impl.NestedWeightedPAutomatons
import wpds.impl.NormalRule
import wpds.impl.Rule
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.impl.WeightedPushdownSystem
import wpds.interfaces.State
import wpds.interfaces.WPAUpdateListener

object AbstractBoomerangSolver {
  private val LOGGER = LoggerFactory.getLogger(classOf[AbstractBoomerangSolver[_ <: Nothing]])

  private class UnbalancedDataFlow[W](private val callee: Nothing, private var trans: Nothing) {
    def getReturningTransition: Nothing = trans

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (callee == null) 0
      else callee.hashCode)
      result = prime * result + (if (trans == null) 0
      else trans.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[AbstractBoomerangSolver.UnbalancedDataFlow[_]]
      if (callee == null) if (other.callee != null) return false
      else if (!callee.equals(other.callee)) return false
      if (trans == null) if (other.trans != null) return false
      else if (!trans.equals(other.trans)) return false
      true
    }
  }

  private class UnbalancedDataFlowListener(private var callee: Nothing, private var callSite: Nothing) {
    def getCallSiteEdge: Nothing = callSite

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (callee == null) 0
      else callee.hashCode)
      result = prime * result + (if (callSite == null) 0
      else callSite.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[AbstractBoomerangSolver.UnbalancedDataFlowListener]
      if (callee == null) if (other.callee != null) return false
      else if (!callee.equals(other.callee)) return false
      if (callSite == null) if (other.callSite != null) return false
      else if (!callSite.equals(other.callSite)) return false
      true
    }
  }
}

abstract class AbstractBoomerangSolver[W <: Weight](protected val icfg: Nothing, protected val cfg: Nothing, protected val generatedFieldState: Nothing, protected val options: Nothing, callSummaries: Nothing, fieldSummaries: Nothing, protected val dataFlowScope: Nothing, protected val `type`: Nothing) extends Nothing(if (icfg.isInstanceOf[Nothing]) false
else options.callSummaries, callSummaries, options.fieldSummaries, fieldSummaries, options.maxCallDepth, options.maxFieldDepth, options.maxUnbalancedCallDepth) {
  this.fieldAutomaton.registerListener((t, w, aut) => {
    addTransitionToMethod(transition.getStart.fact.stmt.getStart.getMethod, transition)
    addTransitionToMethod(transition.getTarget.fact.stmt.getStart.getMethod, transition)
    addTransitionToStatement(transition.getStart.fact.stmt, transition)
  })
  this.callAutomaton.registerListener((t, w, aut) => {
    addCallTransitionToStatement(transition.getLabel, transition, w)
  })
  this.callAutomaton.registerListener(new AbstractBoomerangSolver[W]#UnbalancedListener)
  protected var INTERPROCEDURAL = true
  private val perMethodFieldTransitions = HashMultimap.create
  private val perMethodFieldTransitionsListener = HashMultimap.create
  protected var perStatementFieldTransitions: Nothing = HashMultimap.create
  private val perStatementFieldTransitionsListener = HashMultimap.create
  private val perStatementCallTransitions = HashBasedTable.create
  private val perStatementCallTransitionsListener = HashMultimap.create
  private val unbalancedDataFlows = HashMultimap.create
  private val unbalancedDataFlowListeners = HashMultimap.create

  def reachesNodeWithEmptyField(node: Nothing): Boolean = {
    import scala.collection.JavaConversions._
    for (t <- getFieldAutomaton.getTransitions) {
      if (t.getStart.isInstanceOf[Nothing]) continue //todo: continue is not supported
      if (t.getStart.fact.equals(node) && t.getLabel.equals(Field.empty)) return true
    }
    false
  }

  private class UnbalancedListener extends Nothing {
    @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      if (t.getLabel.equals(new Nothing(Statement.epsilon, Statement.epsilon))) return
      if (icfg.isExitStmt(if (thisAbstractBoomerangSolver.isInstanceOf[Nothing]) t.getLabel.getTarget
      else t.getLabel.getStart)) {
        val exitStmt = t.getLabel.getTarget
        val callee = exitStmt.getMethod
        if (callAutomaton.getInitialStates.contains(t.getTarget)) addPotentialUnbalancedFlow(callee, t, w)
      }
    }
  }

  def createQueryNodeField(query: Nothing) = new Nothing(/* TODO Replace by new designated type */ new Nothing(query.cfgEdge, query.asNode.fact.asUnbalanced(query.cfgEdge)))

  def synchedEmptyStackReachable(sourceNode: Nothing, listener: Nothing): Unit = {
    synchedReachable(sourceNode, new Nothing() {
      private[solver] val potentialFieldCandidate = HashMultimap.create
      private[solver] val potentialCallCandidate = Sets.newHashSet

      @Override def fieldWitness(t: Nothing): Unit = {
        if (t.getTarget.isInstanceOf[Nothing]) return
        if (!t.getLabel.equals(emptyField)) return
        val targetFact = new Nothing(t.getTarget.fact.stmt, t.getTarget.fact.fact.asUnbalanced(null))
        if (!potentialFieldCandidate.put(targetFact.fact, targetFact)) return
        if (potentialCallCandidate.contains(targetFact.fact)) listener.witnessFound(targetFact)
      }

      @Override def callWitness(t: Nothing): Unit = {
        var targetFact = t.getTarget.fact
        if (targetFact.isInstanceOf[Nothing]) {
          targetFact = targetFact.asInstanceOf[Nothing].getDelegate
          if (!potentialCallCandidate.add(targetFact)) return
          if (potentialFieldCandidate.containsKey(targetFact)) {
            import scala.collection.JavaConversions._
            for (w <- potentialFieldCandidate.get(targetFact)) {
              listener.witnessFound(w)
            }
          }
        }
      }
    })
  }

  def synchedReachable(sourceNode: Nothing, listener: Nothing): Unit = {
    registerListener((reachableNode) => {
      def foo(reachableNode) = {
        if (!reachableNode.equals(sourceNode)) return
        fieldAutomaton.registerListener((t, w, aut) => {
          def foo(t, w, aut) = {
            if (t.getStart.isInstanceOf[Nothing]) return
            if (!t.getStart.fact.equals(sourceNode)) return
            listener.fieldWitness(t)
          }

          foo(t, w, aut)
        })
        callAutomaton.registerListener((t, w, aut) => {
          def foo(t, w, aut) = {
            if (t.getStart.isInstanceOf[Nothing]) return
            if (!t.getStart.fact.equals(sourceNode.fact)) return
            if (!t.getLabel.equals(sourceNode.stmt)) return
            if (callAutomaton.isUnbalancedState(t.getTarget)) listener.callWitness(t)
          }

          foo(t, w, aut)
        })
      }

      foo(reachableNode)
    })
  }

  def asStatementValWeightTable: Nothing = {
    val results = HashBasedTable.create
    val sw = Stopwatch.createStarted
    val callAut = getCallAutomaton
    import scala.collection.JavaConversions._
    for (e <- callAut.getTransitionsToFinalWeights.entrySet) {
      val t = e.getKey
      val w = e.getValue
      if (t.getLabel.equals(new Nothing(Statement.epsilon, Statement.epsilon))) continue //todo: continue is not supported
      if (t.getStart.fact.isLocal && !t.getLabel.getMethod.equals(t.getStart.fact.m)) continue //todo: continue is not supported
      results.put(t.getLabel, t.getStart.fact, w)
    }
    results
  }

  protected def addPotentialUnbalancedFlow(callee: Nothing, trans: Nothing, weight: W): Unit = {
    if (unbalancedDataFlows.put(callee, new AbstractBoomerangSolver.UnbalancedDataFlow[W](callee, trans))) {
      val existingListeners = Lists.newArrayList(unbalancedDataFlowListeners.get(callee))
      import scala.collection.JavaConversions._
      for (l <- existingListeners) {
        propagateUnbalancedToCallSite(l.getCallSiteEdge, trans)
      }
    }
    if (forceUnbalanced(trans.getTarget, callAutomaton.getUnbalancedStartOf(trans.getTarget))) icfg.addCallerListener(new Nothing() {
      @Override def getObservedCallee: Nothing = callee

      @Override def onCallerAdded(n: Nothing, m: Nothing): Unit = {
        propagateUnbalancedToCallSite(n, trans)
      }
    })
  }

  protected def forceUnbalanced(iNode: Nothing, collection: Nothing) = false

  def allowUnbalanced(callee: Nothing, callSite: Nothing): Unit = {
    if (dataFlowScope.isExcluded(callee)) return
    val l = new AbstractBoomerangSolver.UnbalancedDataFlowListener(callee, callSite)
    if (unbalancedDataFlowListeners.put(callee, l)) {
      AbstractBoomerangSolver.LOGGER.trace("Allowing unbalanced propagation from {} to {} of {}", callee, callSite, this)
      import scala.collection.JavaConversions._
      for (e <- Lists.newArrayList(unbalancedDataFlows.get(callee))) {
        propagateUnbalancedToCallSite(callSite, e.getReturningTransition)
      }
    }
  }

  protected def isMatchingCallSiteCalleePair(callSite: Nothing, method: Nothing): Boolean = {
    val callsitesOfCall = Sets.newHashSet
    icfg.addCallerListener(new Nothing() {
      @Override def getObservedCallee: Nothing = method

      @Override def onCallerAdded(statement: Nothing, method: Nothing): Unit = {
        callsitesOfCall.add(statement)
      }
    })
    callsitesOfCall.contains(callSite)
  }

  protected def propagateUnbalancedToCallSite(callSiteEdge: Nothing, transInCallee: Nothing): Unit

  @Override protected def preventCallTransitionAdd(t: Nothing, weight: W) = false

  @Override def addCallRule(rule: Nothing): Unit = {
    if (rule.isInstanceOf[Nothing]) if (rule.getL1.equals(rule.getL2) && rule.getS1.equals(rule.getS2)) return
    super.addCallRule(rule)
  }

  @Override def addFieldRule(rule: Nothing): Unit = {
    if (rule.isInstanceOf[Nothing]) if (rule.getL1.equals(rule.getL2) && rule.getS1.equals(rule.getS2)) return
    super.addFieldRule(rule)
  }

  private def addTransitionToMethod(method: Nothing, t: Nothing): Unit = {
    if (perMethodFieldTransitions.put(method, t)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(perMethodFieldTransitionsListener.get(method))) {
        l.onAddedTransition(t)
      }
    }
  }

  def registerFieldTransitionListener(l: Nothing): Unit = {
    if (perMethodFieldTransitionsListener.put(l.getMethod, l)) {
      import scala.collection.JavaConversions._
      for (t <- Lists.newArrayList(perMethodFieldTransitions.get(l.getMethod))) {
        l.onAddedTransition(t)
      }
    }
  }

  private def addTransitionToStatement(s: Nothing, t: Nothing): Unit = {
    if (perStatementFieldTransitions.put(s, t)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(perStatementFieldTransitionsListener.get(s))) {
        l.onAddedTransition(t)
      }
    }
  }

  def registerStatementFieldTransitionListener(l: Nothing): Unit = {
    if (perStatementFieldTransitionsListener.put(l.getCfgEdge, l)) {
      import scala.collection.JavaConversions._
      for (t <- Lists.newArrayList(perStatementFieldTransitions.get(l.getCfgEdge))) {
        l.onAddedTransition(t)
      }
    }
  }

  private def addCallTransitionToStatement(s: Nothing, t: Nothing, w: W): Unit = {
    val put = perStatementCallTransitions.get(s, t)
    if (put != null) {
      val combineWith = put.combineWith(w).asInstanceOf[W]
      if (!combineWith.equals(put)) {
        perStatementCallTransitions.put(s, t, combineWith)
        import scala.collection.JavaConversions._
        for (l <- Lists.newArrayList(perStatementCallTransitionsListener.get(s))) {
          l.onAddedTransition(t, w)
        }
      }
    }
    else {
      perStatementCallTransitions.put(s, t, w)
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(perStatementCallTransitionsListener.get(s))) {
        l.onAddedTransition(t, w)
      }
    }
  }

  def registerStatementCallTransitionListener(l: Nothing): Unit = {
    if (perStatementCallTransitionsListener.put(l.getControlFlowEdge, l)) {
      val row = perStatementCallTransitions.row(l.getControlFlowEdge)
      import scala.collection.JavaConversions._
      for (t <- Lists.newArrayList(row.entrySet)) {
        l.onAddedTransition(t.getKey, t.getValue)
      }
    }
  }

  def generateFieldState(d: Nothing, loc: Nothing): Nothing = {
    val e = new Nothing(d, loc)
    if (!generatedFieldState.containsKey(e)) generatedFieldState.put(e, new Nothing(d, loc))
    generatedFieldState.get(e)
  }

  private def isBackward = this.isInstanceOf[Nothing]

  protected def computeReturnFlow(method: Nothing, curr: Nothing, value: Nothing): Nothing

  protected def returnFlow(method: Nothing, currNode: Nothing): Unit = {
    val value = currNode.fact
    val outFlow = computeReturnFlow(method, currNode.stmt.getTarget, value)
    import scala.collection.JavaConversions._
    for (s <- outFlow) {
      propagate(currNode, s)
    }
  }

  protected def computeNormalFlow(method: Nothing, currEdge: Nothing, value: Nothing): Nothing

  @Override def epsilonField: Nothing = Field.epsilon

  @Override def emptyField: Nothing = Field.empty

  @Override def epsilonStmt = new Nothing(Statement.epsilon, Statement.epsilon)

  @Override def fieldWildCard: Nothing = Field.wildcard

  @Override def exclusionFieldWildCard(exclusion: Nothing): Nothing = Field.exclusionWildcard(exclusion)

  def getFieldAutomaton: Nothing = fieldAutomaton

  def getCallAutomaton: Nothing = callAutomaton

  def getCallPDS: Nothing = callingPDS

  def getFieldPDS: Nothing = fieldPDS

  def getNumberOfRules: Int = callingPDS.getAllRules.size + fieldPDS.getAllRules.size

  @Override protected def preventFieldTransitionAdd(t: Nothing, weight: W): Boolean = {
    if (t.getStart.equals(t.getTarget) && t.getLabel.equals(Field.empty)) {
      AbstractBoomerangSolver.LOGGER.warn("Prevented illegal edge addition of {}", t)
      return true
    }
    if (!t.getLabel.equals(Field.empty) || !options.typeCheck) return false
    if (t.getTarget.isInstanceOf[Nothing] || t.getStart.isInstanceOf[Nothing]) return false
    val target = t.getTarget.fact.fact
    val source = t.getStart.fact.fact
    if (source.isStatic) return false
    val sourceVal = source.getType
    val targetVal = if (isBackward) target.getType
    else `type`
    if (sourceVal == null) return true
    if (sourceVal.equals(targetVal)) return false
    if (!targetVal.isRefType || !sourceVal.isRefType) {
      if (options.killNullAtCast && targetVal.isNullType && isCastNode(t.getStart.fact)) {
        // A null pointer cannot be cast to any object
        return true
      }
      return false // !allocVal.value().getType().equals(varVal.value().getType());
    }
    sourceVal.doesCastFail(targetVal, target)
  }

  private def isCastNode(node: Nothing): Boolean = {
    val isCast = node.stmt.getStart.isCast
    if (isCast) {
      val rightOp = node.stmt.getStart.getRightOp
      if (rightOp.isCast) if (rightOp.getCastOp.equals(node.fact)) return true
    }
    false
  }

  def getResultsAt(stmt: Nothing): Nothing = {
    val results = Maps.newHashMap
    fieldAutomaton.registerListener((t, w, aut) => {
      def foo(t, w, aut) = {
        if (t.getStart.isInstanceOf[Nothing]) return
        if (t.getStart.fact.stmt.equals(stmt)) {
          import scala.collection.JavaConversions._
          for (initState <- fieldAutomaton.getInitialStates) {
            val regEx = fieldAutomaton.toRegEx(t.getStart, initState)
            results.put(new Nothing(t.getStart.fact.fact, regEx), w)
          }
        }
      }

      foo(t, w, aut)
    })
    results
  }

  def getResults(m: Nothing): Nothing = {
    val results = HashBasedTable.create
    AbstractBoomerangSolver.LOGGER.debug("Start extracting results from {}", this)
    fieldAutomaton.registerListener((t, w, aut) => {
      def foo(t, w, aut) = {
        if (t.getStart.isInstanceOf[Nothing]) return
        if (t.getStart.fact.stmt.getStart.getMethod.equals(m)) {
          import scala.collection.JavaConversions._
          for (initState <- fieldAutomaton.getInitialStates) {
            val regEx = fieldAutomaton.toRegEx(t.getStart, initState)
            thisAbstractBoomerangSolver.callAutomaton.registerListener((callT, w1, aut1) => {
              if (callT.getStart.fact.equals(t.getStart.fact.fact) && callT.getLabel.equals(t.getStart.fact.stmt)) results.put(t.getStart.fact.stmt, new Nothing(t.getStart.fact.fact, regEx), w1)
            })
          }
        }
      }

      foo(t, w, aut)
    })
    AbstractBoomerangSolver.LOGGER.debug("End extracted results from {}", this)
    results
  }

  def debugFieldAutomaton(stmt: Nothing): Unit = {
    fieldAutomaton.registerListener((t, w, aut) => {
      def foo(t, w, aut) = {
        if (t.getStart.isInstanceOf[Nothing]) return
        if (t.getStart.fact.stmt.equals(stmt)) {
          import scala.collection.JavaConversions._
          for (initState <- fieldAutomaton.getInitialStates) {
            val regEx = fieldAutomaton.toRegEx(t.getStart, initState)
            AbstractBoomerangSolver.LOGGER.debug(t.getStart.fact.fact + " " + regEx)
          }
        }
      }

      foo(t, w, aut)
    })
  }

  def unregisterAllListeners(): Unit = {
    this.callAutomaton.unregisterAllListeners
    this.fieldAutomaton.unregisterAllListeners
    this.perMethodFieldTransitionsListener.clear
    this.perStatementCallTransitionsListener.clear
    this.perStatementFieldTransitionsListener.clear
    this.unbalancedDataFlowListeners.clear
    this.unbalancedDataFlows.clear
    this.callingPDS.unregisterAllListeners
    this.fieldPDS.unregisterAllListeners
  }
}