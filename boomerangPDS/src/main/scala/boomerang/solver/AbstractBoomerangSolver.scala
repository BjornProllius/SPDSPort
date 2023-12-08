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
import java.util.AbstractMap
import java.util.Collection
import java.util.Map
import java.util.Map.Entry
import java.util.Set
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

abstract class AbstractBoomerangSolver[W <: Weight]
    extends SyncPDSSolver[ControlFlowGraph.Edge, Val, Field, W] {

  private val LOGGER = LoggerFactory.getLogger(classOf[AbstractBoomerangSolver[_]])

  protected val icfg: ObservableICFG[Statement, Method]
  protected val cfg: ObservableControlFlowGraph
  protected var INTERPROCEDURAL = true
  protected val generatedFieldState: Map[
    Entry[INode[Node[ControlFlowGraph.Edge, Val]], Field],
    INode[Node[ControlFlowGraph.Edge, Val]]
  ]
  private var perMethodFieldTransitions = HashMultimap.create[Method, Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]]]()
  private var perMethodFieldTransitionsListener = HashMultimap.create[Method, MethodBasedFieldTransitionListener[W]]()
  protected var perStatementFieldTransitions = HashMultimap.create[ControlFlowGraph.Edge, Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]]]()
  private var perStatementFieldTransitionsListener = HashMultimap.create[ControlFlowGraph.Edge, ControlFlowEdgeBasedFieldTransitionListener[W]]()
  private var perStatementCallTransitions = HashBasedTable.create[ControlFlowGraph.Edge, Transition[ControlFlowGraph.Edge, INode[Val]], W]()
  private var perStatementCallTransitionsListener = HashMultimap.create[ControlFlowGraph.Edge, ControlFlowEdgeBasedCallTransitionListener[W]]()
  private var unbalancedDataFlows = HashMultimap.create[Method, UnbalancedDataFlow[W]]()
  private var unbalancedDataFlowListeners = HashMultimap.create[Method, UnbalancedDataFlowListener]()
  protected val dataFlowScope: DataFlowScope
  protected val options: BoomerangOptions
  protected val `type`: Type

  def this( // was initially AstractBoomerangSolver
      icfg: ObservableICFG[Statement, Method],
      cfg: ObservableControlFlowGraph,
      genField: Map[Entry[INode[Node[Edge, Val]], Field], INode[Node[Edge, Val]]],
      options: BoomerangOptions,
      callSummaries: NestedWeightedPAutomatons[ControlFlowGraph.Edge, INode[Val], W],
      fieldSummaries: NestedWeightedPAutomatons[Field, INode[Node[ControlFlowGraph.Edge, Val]], W],
      scope: DataFlowScope,
      propagationType: Type) {
    super(
      if (icfg.isInstanceOf[BackwardsObservableICFG]) false else options.callSummaries(),
      callSummaries,
      options.fieldSummaries(),
      fieldSummaries,
      options.maxCallDepth(),
      options.maxFieldDepth(),
      options.maxUnbalancedCallDepth()
    )
    this.options = options
    this.icfg = icfg
    this.cfg = cfg
    this.dataFlowScope = scope
    this.`type` = propagationType
    this.fieldAutomaton.registerListener(
      (t, w, aut) => {
        addTransitionToMethod(t.getStart().fact().stmt().getStart().getMethod(), t)
        addTransitionToMethod(t.getTarget().fact().stmt().getStart().getMethod(), t)
        addTransitionToStatement(t.getStart().fact().stmt(), t)
      }
    )
    this.callAutomaton.registerListener(
      (t, w, aut) => {
        addCallTransitionToStatement(t.getLabel(), t, w)
      }
    )
    this.callAutomaton.registerListener(new UnbalancedListener())
    this.generatedFieldState = genField
  }

  def reachesNodeWithEmptyField(node: Node[Edge, Val]): Boolean = {
    for (t <- getFieldAutomaton().getTransitions) {
      if (t.getStart.isInstanceOf[GeneratedState]) {
        // continue
      } else if (t.getStart.fact.equals(node) && t.getLabel.equals(Field.empty)) {
        return true
      }
    }
    false
  }

  private class UnbalancedListener extends WPAUpdateListener[Edge, INode[Val], W] {
    override def onWeightAdded(t: Transition[Edge, INode[Val]], w: W, aut: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
      if (t.getLabel.equals(new Edge(Statement.epsilon, Statement.epsilon))) return
      if (icfg.isExitStmt(
        if (AbstractBoomerangSolver.this.isInstanceOf[ForwardBoomerangSolver])
          t.getLabel.getTarget
        else
          t.getLabel.getStart)) {
        val exitStmt = t.getLabel.getTarget
        val callee = exitStmt.getMethod
        if (callAutomaton.getInitialStates.contains(t.getTarget)) {
          addPotentialUnbalancedFlow(callee, t, w)
        }
      }
    }
  }

  def createQueryNodeField(query: Query): INode[Node[Edge, Val]] = {
    new SingleNode(
      new Node(
        query.cfgEdge, query.asNode.fact.asUnbalanced(query.cfgEdge)))
  }

  def synchedEmptyStackReachable(sourceNode: Node[Edge, Val], listener: EmptyStackWitnessListener[Edge, Val]): Unit = {
    synchedReachable(
      sourceNode,
      new WitnessListener[Edge, Val, Field] {
        val potentialFieldCandidate = HashMultimap.create[Val, Node[Edge, Val]]
        val potentialCallCandidate = Sets.newHashSet[Val]

        override def fieldWitness(t: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
          if (t.getTarget.isInstanceOf[GeneratedState]) return
          if (!t.getLabel.equals(emptyField)) return
          val targetFact =
            new Node(
              t.getTarget.fact.stmt, t.getTarget.fact.fact.asUnbalanced(null))
          if (!potentialFieldCandidate.put(targetFact.fact, targetFact)) return
          if (potentialCallCandidate.contains(targetFact.fact)) {
            listener.witnessFound(targetFact)
          }
        }

        override def callWitness(t: Transition[Edge, INode[Val]]): Unit = {
          var targetFact = t.getTarget.fact
          if (targetFact.isInstanceOf[AllocVal]) {
            targetFact = targetFact.asInstanceOf[AllocVal].getDelegate
            if (!potentialCallCandidate.add(targetFact)) return
            if (potentialFieldCandidate.containsKey(targetFact)) {
              for (w <- potentialFieldCandidate.get(targetFact)) {
                listener.witnessFound(w)
              }
            }
          }
        }
      })
  }

  def reachesNodeWithEmptyField(node: Node[Edge, Val]): Boolean = {
    for (t <- getFieldAutomaton().getTransitions) {
      if (t.getStart.isInstanceOf[GeneratedState]) {
        // continue
      } else if (t.getStart.fact.equals(node) && t.getLabel.equals(Field.empty)) {
        return true
      }
    }
    false
  }

  private class UnbalancedListener extends WPAUpdateListener[Edge, INode[Val], W] {
    override def onWeightAdded(t: Transition[Edge, INode[Val]], w: W, aut: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
      if (t.getLabel.equals(new Edge(Statement.epsilon, Statement.epsilon))) return
      if (icfg.isExitStmt(
        if (AbstractBoomerangSolver.this.isInstanceOf[ForwardBoomerangSolver])
          t.getLabel.getTarget
        else
          t.getLabel.getStart)) {
        val exitStmt = t.getLabel.getTarget
        val callee = exitStmt.getMethod
        if (callAutomaton.getInitialStates.contains(t.getTarget)) {
          addPotentialUnbalancedFlow(callee, t, w)
        }
      }
    }
  }

  def createQueryNodeField(query: Query): INode[Node[Edge, Val]] = {
    new SingleNode(
      new Node(
        query.cfgEdge, query.asNode.fact.asUnbalanced(query.cfgEdge)))
  }

  def synchedEmptyStackReachable(sourceNode: Node[Edge, Val], listener: EmptyStackWitnessListener[Edge, Val]): Unit = {
    synchedReachable(
      sourceNode,
      new WitnessListener[Edge, Val, Field] {
        val potentialFieldCandidate = HashMultimap.create[Val, Node[Edge, Val]]
        val potentialCallCandidate = Sets.newHashSet[Val]

        override def fieldWitness(t: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
          if (t.getTarget.isInstanceOf[GeneratedState]) return
          if (!t.getLabel.equals(emptyField)) return
          val targetFact =
            new Node(
              t.getTarget.fact.stmt, t.getTarget.fact.fact.asUnbalanced(null))
          if (!potentialFieldCandidate.put(targetFact.fact, targetFact)) return
          if (potentialCallCandidate.contains(targetFact.fact)) {
            listener.witnessFound(targetFact)
          }
        }

        override def callWitness(t: Transition[Edge, INode[Val]]): Unit = {
          var targetFact = t.getTarget.fact
          if (targetFact.isInstanceOf[AllocVal]) {
            targetFact = targetFact.asInstanceOf[AllocVal].getDelegate
            if (!potentialCallCandidate.add(targetFact)) return
            if (potentialFieldCandidate.containsKey(targetFact)) {
              for (w <- potentialFieldCandidate.get(targetFact)) {
                listener.witnessFound(w)
              }
            }
          }
        }
      })
  }

  protected def forceUnbalanced(iNode: INode[Val], collection: Collection[INode[Val]]): Boolean = {
    false
  }

  def allowUnbalanced(callee: Method, callSite: Statement): Unit = {
    if (dataFlowScope.isExcluded(callee)) {
      return
    }
    val l = new UnbalancedDataFlowListener(callee, callSite)
    if (unbalancedDataFlowListeners.put(callee, l)) {
      LOGGER.trace("Allowing unbalanced propagation from {} to {} of {}", callee, callSite, this)
      for (e <- Lists.newArrayList(unbalancedDataFlows.get(callee))) {
        propagateUnbalancedToCallSite(callSite, e.getReturningTransition())
      }
    }
  }

  protected def isMatchingCallSiteCalleePair(callSite: Statement, method: Method): Boolean = {
    val callsitesOfCall = Sets.newHashSet[Statement]()
    icfg.addCallerListener(
      new CallerListener[Statement, Method] {
        override def getObservedCallee(): Method = {
          method
        }

        override def onCallerAdded(statement: Statement, method: Method): Unit = {
          callsitesOfCall.add(statement)
        }
      }
    )
    callsitesOfCall.contains(callSite)
  }

  protected def propagateUnbalancedToCallSite(
      callSiteEdge: Statement, transInCallee: Transition[ControlFlowGraph.Edge, INode[Val]]): Unit

  override protected def preventCallTransitionAdd(
      t: Transition[ControlFlowGraph.Edge, INode[Val]], weight: W): Boolean = {
    false
  }

  override def addCallRule(rule: Rule[ControlFlowGraph.Edge, INode[Val], W]): Unit = {
    if (rule.isInstanceOf[NormalRule]) {
      if (rule.getL1().equals(rule.getL2()) && rule.getS1().equals(rule.getS2())) return
    }
    super.addCallRule(rule)
  }

  override def addFieldRule(rule: Rule[Field, INode[Node[ControlFlowGraph.Edge, Val]], W]): Unit = {
    if (rule.isInstanceOf[NormalRule]) {
      if (rule.getL1().equals(rule.getL2()) && rule.getS1().equals(rule.getS2())) return
    }
    super.addFieldRule(rule)
  }

  private def addTransitionToMethod(method: Method, t: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
    if (perMethodFieldTransitions.put(method, t)) {
      for (l <- Lists.newArrayList(perMethodFieldTransitionsListener.get(method))) {
        l.onAddedTransition(t)
      }
    }
  }

  def registerFieldTransitionListener(l: MethodBasedFieldTransitionListener[W]): Unit = {
    if (perMethodFieldTransitionsListener.put(l.getMethod, l)) {
      for (t <- Lists.newArrayList(perMethodFieldTransitions.get(l.getMethod))) {
        l.onAddedTransition(t)
      }
    }
  }

  private def addTransitionToStatement(s: ControlFlowGraph.Edge, t: Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]]): Unit = {
    if (perStatementFieldTransitions.put(s, t)) {
      for (l <- Lists.newArrayList(perStatementFieldTransitionsListener.get(s))) {
        l.onAddedTransition(t)
      }
    }
  }

  def registerStatementFieldTransitionListener(l: ControlFlowEdgeBasedFieldTransitionListener[W]): Unit = {
    if (perStatementFieldTransitionsListener.put(l.getCfgEdge, l)) {
      for (t <- Lists.newArrayList(perStatementFieldTransitions.get(l.getCfgEdge))) {
        l.onAddedTransition(t)
      }
    }
  }

  private def addCallTransitionToStatement(s: Edge, t: Transition[Edge, INode[Val]], w: W): Unit = {
    val put = perStatementCallTransitions.get(s, t)
    if (put != null) {
      val combineWith = put.combineWith(w).asInstanceOf[W]
      if (!combineWith.equals(put)) {
        perStatementCallTransitions.put(s, t, combineWith)
        for (l <- Lists.newArrayList(perStatementCallTransitionsListener.get(s))) {
          l.onAddedTransition(t, w)
        }
      }
    } else {
      perStatementCallTransitions.put(s, t, w)
      for (l <- Lists.newArrayList(perStatementCallTransitionsListener.get(s))) {
        l.onAddedTransition(t, w)
      }
    }
  }

  def registerStatementCallTransitionListener(l: ControlFlowEdgeBasedCallTransitionListener[W]): Unit = {
    if (perStatementCallTransitionsListener.put(l.getControlFlowEdge, l)) {
      val row = perStatementCallTransitions.row(l.getControlFlowEdge)
      for (t <- Lists.newArrayList(row.entrySet)) {
        l.onAddedTransition(t.getKey, t.getValue)
      }
    }
  }

  def generateFieldState(d: INode[Node[ControlFlowGraph.Edge, Val]], loc: Field): INode[Node[ControlFlowGraph.Edge, Val]] = {
    val e = new AbstractMap.SimpleEntry(d, loc)
    if (!generatedFieldState.containsKey(e)) {
      generatedFieldState.put(e, new GeneratedState(d, loc))
    }
    generatedFieldState.get(e)
  }

  private def isBackward(): Boolean = {
    this.isInstanceOf[BackwardBoomerangSolver]
  }

  protected def computeReturnFlow(method: Method, curr: Statement, value: Val): Collection[_ <: State]

  protected def returnFlow(method: Method, currNode: Node[ControlFlowGraph.Edge, Val]): Unit = {
    val value = currNode.fact()

    val outFlow = computeReturnFlow(method, currNode.stmt().getTarget, value)

    for (s <- outFlow) {
      propagate(currNode, s)
    }
  }

  protected def computeNormalFlow(method: Method, currEdge: Edge, value: Val): Collection[State]

  override def epsilonField(): Field = {
    Field.epsilon()
  }

  override def emptyField(): Field = {
    Field.empty()
  }

  override def epsilonStmt(): ControlFlowGraph.Edge = {
    new Edge(Statement.epsilon(), Statement.epsilon())
  }

  override def fieldWildCard(): Field = {
    Field.wildcard()
  }

  override def exclusionFieldWildCard(exclusion: Field): Field = {
    Field.exclusionWildcard(exclusion)
  }

  def getFieldAutomaton(): WeightedPAutomaton[Field, INode[Node[ControlFlowGraph.Edge, Val]], W] = {
    fieldAutomaton
  }

  def getCallAutomaton(): WeightedPAutomaton[ControlFlowGraph.Edge, INode[Val], W] = {
    callAutomaton
  }

  def getCallPDS(): WeightedPushdownSystem[ControlFlowGraph.Edge, INode[Val], W] = {
    callingPDS
  }

  def getFieldPDS(): WeightedPushdownSystem[Field, INode[Node[ControlFlowGraph.Edge, Val]], W] = {
    fieldPDS
  }

  def getNumberOfRules(): Int = {
    callingPDS.getAllRules().size + fieldPDS.getAllRules().size
  }

  override protected def preventFieldTransitionAdd(t: Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]], weight: W): Boolean = {
    if (t.getStart().equals(t.getTarget()) && t.getLabel().equals(Field.empty())) {
      LOGGER.warn("Prevented illegal edge addition of {}", t)
      return true
    }
    if (!t.getLabel().equals(Field.empty()) || !options.typeCheck()) {
      return false
    }
    if (t.getTarget().isInstanceOf[GeneratedState] || t.getStart().isInstanceOf[GeneratedState]) {
      return false
    }
    val target = t.getTarget().fact().fact()
    val source = t.getStart().fact().fact()

    if (source.isStatic()) {
      return false
    }

    val sourceVal = source.getType()
    val targetVal = if (isBackward()) target.getType() else `type`
    if (sourceVal == null) {
      return true
    }
    if (sourceVal.equals(targetVal)) {
      return false
    }
    if (!(targetVal.isRefType()) || !(sourceVal.isRefType())) {
      if (options.killNullAtCast() && targetVal.isNullType() && isCastNode(t.getStart().fact())) {
        // A null pointer cannot be cast to any object
        return true
      }
      return false
    }
    sourceVal.doesCastFail(targetVal, target)
  }

  private def isCastNode(node: Node[ControlFlowGraph.Edge, Val]): Boolean = {
    val isCast = node.stmt().getStart().isCast()
    if (isCast) {
      val rightOp = node.stmt().getStart().getRightOp()
      if (rightOp.isCast()) {
        if (rightOp.getCastOp().equals(node.fact())) {
          return true
        }
      }
    }
    false
  }

  def getResultsAt(stmt: Statement): Map[RegExAccessPath, W] = {
    val results = Maps.newHashMap[RegExAccessPath, W]()
    fieldAutomaton.registerListener(
      (t, w, aut) => {
        if (t.getStart().isInstanceOf[GeneratedState]) {
          return
        }
        if (t.getStart().fact().stmt().equals(stmt)) {
          for (initState <- fieldAutomaton.getInitialStates()) {
            val regEx = fieldAutomaton.toRegEx(t.getStart(), initState)

            results.put(new RegExAccessPath(t.getStart().fact().fact(), regEx), w)
          }
        }
      }
    )
    results
  }

  def getResults(m: Method): Table[Edge, RegExAccessPath, W] = {
    val results = HashBasedTable.create[Edge, RegExAccessPath, W]()
    LOGGER.debug("Start extracting results from {}", this)
    fieldAutomaton.registerListener(
      (t, w, aut) => {
        if (t.getStart().isInstanceOf[GeneratedState]) {
          return
        }
        if (t.getStart().fact().stmt().getStart().getMethod().equals(m)) {
          for (initState <- fieldAutomaton.getInitialStates()) {
            val regEx = fieldAutomaton.toRegEx(t.getStart(), initState)
            this.callAutomaton.registerListener(
              (callT, w1, aut1) => {
                if (callT.getStart().fact().equals(t.getStart().fact().fact())
                    && callT.getLabel().equals(t.getStart().fact().stmt())) {
                  results.put(
                    t.getStart().fact().stmt(),
                    new RegExAccessPath(t.getStart().fact().fact(), regEx),
                    w1)
                }
              })
          }
        }
      })
    LOGGER.debug("End extracted results from {}", this)
    results
  }

  def debugFieldAutomaton(stmt: Statement): Unit = {
    fieldAutomaton.registerListener(
      (t, w, aut) => {
        if (t.getStart().isInstanceOf[GeneratedState]) {
          return
        }
        if (t.getStart().fact().stmt().equals(stmt)) {
          for (initState <- fieldAutomaton.getInitialStates()) {
            val regEx = fieldAutomaton.toRegEx(t.getStart(), initState)
            LOGGER.debug(t.getStart().fact().fact() + " " + regEx)
          }
        }
      })
  }

  def unregisterAllListeners(): Unit = {
    this.callAutomaton.unregisterAllListeners()
    this.fieldAutomaton.unregisterAllListeners()
    this.perMethodFieldTransitionsListener.clear()
    this.perStatementCallTransitionsListener.clear()
    this.perStatementFieldTransitionsListener.clear()
    this.unbalancedDataFlowListeners.clear()
    this.unbalancedDataFlows.clear()
    this.callingPDS.unregisterAllListeners()
    this.fieldPDS.unregisterAllListeners()
  }

  private class UnbalancedDataFlow[W](val callee: Method, var trans: Transition[ControlFlowGraph.Edge, INode[Val]]) {

    def getReturningTransition(): Transition[ControlFlowGraph.Edge, INode[Val]] = trans

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (callee == null) 0 else callee.hashCode())
      result = prime * result + (if (trans == null) 0 else trans.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: UnbalancedDataFlow[_] =>
        (this eq other) || (other != null && getClass == other.getClass &&
          (if (callee == null) other.callee == null else callee.equals(other.callee)) &&
          (if (trans == null) other.trans == null else trans.equals(other.trans)))
      case _ => false
    }
  }

  private class UnbalancedDataFlowListener(val callee: Method, val callSite: Statement) {

    def getCallSiteEdge(): Statement = callSite

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (callee == null) 0 else callee.hashCode())
      result = prime * result + (if (callSite == null) 0 else callSite.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: UnbalancedDataFlowListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          (if (callee == null) other.callee == null else callee.equals(other.callee)) &&
          (if (callSite == null) other.callSite == null else callSite.equals(other.callSite)))
      case _ => false
    }
  }
}