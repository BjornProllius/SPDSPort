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
package sync.pds.solver

import com.google.common.base.Objects
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import java.util.Map.Entry
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.ExclusionNode
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.NodeWithLocation
import sync.pds.solver.nodes.PopNode
import sync.pds.solver.nodes.PushNode
import sync.pds.solver.nodes.SingleNode
import wpds.impl.NestedAutomatonListener
import wpds.impl.NestedWeightedPAutomatons
import wpds.impl.NormalRule
import wpds.impl.PopRule
import wpds.impl.PushRule
import wpds.impl.Rule
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.impl.WeightedPushdownSystem
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.WPAStateListener
import wpds.interfaces.WPAUpdateListener

object SyncPDSSolver {
  object PDSSystem extends Enumeration {
    type PDSSystem = Value
    val FIELDS, CALLS = Value
  }

  private val logger = LoggerFactory.getLogger(classOf[SyncPDSSolver[_ <: Nothing, _, _ <: Nothing, _ <: Nothing]])
  private val FieldSensitive = true
  private val ContextSensitive = true

  trait OnAddedSummaryListener[Stmt, Fact] {
    def apply(callSite: Stmt, factInCallee: Fact, spInCallee: Stmt, exitStmt: Stmt, returnedFact: Fact): Unit
  }
}

abstract class SyncPDSSolver[Stmt <: Location, Fact, Field <: Location, W <: Weight](useCallSummaries: Boolean, callSummaries: Nothing, useFieldSummaries: Boolean, fieldSummaries: Nothing, maxCallDepth: Int, maxFieldDepth: Int, maxUnbalancedCallDepth: Int) {
  fieldAutomaton = new Nothing() {
    @Override def createState(d: Nothing, loc: Field): Nothing = {
      if (loc.equals(emptyField)) return d
      generateFieldState(d, loc)
    }

    @Override def epsilon: Field = epsilonField

    @Override def nested: Boolean = useFieldSummaries

    @Override def getOne: W = getFieldWeights.getOne

    @Override def getMaxDepth: Int = maxFieldDepth

    def addWeightForTransition(trans: Nothing, weight: W): Boolean = {
      if (preventFieldTransitionAdd(trans, weight)) return false
      SyncPDSSolver.logger.trace("Adding field transition {} with weight {}", trans, weight)
      super.addWeightForTransition(trans, weight)
    }

    @Override def isGeneratedState(d: Nothing): Boolean = d.isInstanceOf[Nothing]
  }
  callAutomaton = new Nothing() {
    @Override def createState(d: Nothing, loc: Stmt): Nothing = generateCallState(d, loc)

    @Override def epsilon: Stmt = epsilonStmt

    @Override def nested: Boolean = useCallSummaries

    @Override def getOne: W = getCallWeights.getOne

    def addWeightForTransition(trans: Nothing, weight: W): Boolean = {
      if (preventCallTransitionAdd(trans, weight)) return false
      SyncPDSSolver.logger.trace("Adding call transition {} with weight {}", trans, weight)
      super.addWeightForTransition(trans, weight)
    }

    @Override def isGeneratedState(d: Nothing): Boolean = d.isInstanceOf[Nothing]

    @Override def getMaxDepth: Int = maxCallDepth

    @Override def getMaxUnbalancedDepth: Int = maxUnbalancedCallDepth
  }
  callAutomaton.registerListener(new SyncPDSSolver[Stmt, Fact, Field, W]#CallAutomatonListener)
  fieldAutomaton.registerListener(new SyncPDSSolver[Stmt, Fact, Field, W]#FieldUpdateListener)
  if (callAutomaton.nested) callAutomaton.registerNestedAutomatonListener(new SyncPDSSolver[Stmt, Fact, Field, W]#CallSummaryListener)
  // if(fieldAutomaton.nested())
  // fieldAutomaton.registerNestedAutomatonListener(new FieldSummaryListener());
  callingPDS.poststar(callAutomaton, callSummaries)
  fieldPDS.poststar(fieldAutomaton, fieldSummaries)
  final protected val callingPDS = new Nothing() {
    def toString: Nothing = "Call " + thisSyncPDSSolver.toString
  }
  final protected val fieldPDS = new Nothing() {
    def toString: Nothing = "Field " + thisSyncPDSSolver.toString
  }
  final private val reachedStates = Sets.newHashSet
  final private val callingContextReachable = Sets.newHashSet
  final private val fieldContextReachable = Sets.newHashSet
  final private val updateListeners = Sets.newHashSet
  final private val reachedStateUpdateListeners = HashMultimap.create
  final protected var fieldAutomaton: Nothing = null
  final protected var callAutomaton: Nothing = null

  protected def preventFieldTransitionAdd(trans: Nothing, weight: W) = false

  protected def preventCallTransitionAdd(trans: Nothing, weight: W) = false

  private class FieldSummaryListener extends Nothing {
    @Override def nestedAutomaton(parent: Nothing, child: Nothing): Unit = {
      import scala.collection.JavaConversions._
      for (s <- child.getInitialStates) {
        child.registerListener(new SyncPDSSolver[Stmt, Fact, Field, W]#FieldAddEpsilonToInitialStateListener(s, parent))
      }
    }
  }

  private class FieldAddEpsilonToInitialStateListener(state: Nothing, private var parent: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def onInTransitionAdded(nestedT: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (nestedT.getLabel.equals(fieldAutomaton.epsilon)) parent.registerListener(new SyncPDSSolver[Stmt, Fact, Field, W]#FieldOnOutTransitionAddToStateListener(this.getState, nestedT))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (parent == null) 0
      else parent.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[SyncPDSSolver[Stmt, Fact, Field, W]#FieldAddEpsilonToInitialStateListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (parent == null) if (other.parent != null) return false
      else if (!parent.equals(other.parent)) return false
      true
    }

    private def getOuterType = thisSyncPDSSolver
  }

  private class FieldOnOutTransitionAddToStateListener(state: Nothing, private var nestedT: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      setFieldContextReachable(nestedT.getStart.fact)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (nestedT == null) 0
      else nestedT.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[SyncPDSSolver[Stmt, Fact, Field, W]#FieldOnOutTransitionAddToStateListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (nestedT == null) if (other.nestedT != null) return false
      else if (!nestedT.equals(other.nestedT)) return false
      true
    }

    private def getOuterType = thisSyncPDSSolver
  }

  private class CallSummaryListener extends Nothing {
    @Override def nestedAutomaton(parent: Nothing, child: Nothing): Unit = {
      import scala.collection.JavaConversions._
      for (s <- child.getInitialStates) {
        child.registerListener(new SyncPDSSolver[Stmt, Fact, Field, W]#AddEpsilonToInitialStateListener(s, parent))
      }
    }
  }

  private class AddEpsilonToInitialStateListener(state: Nothing, private var parent: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def onInTransitionAdded(nestedT: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (nestedT.getLabel.equals(callAutomaton.epsilon)) parent.registerListener(new SyncPDSSolver[Stmt, Fact, Field, W]#OnOutTransitionAddToStateListener(this.getState, nestedT))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (parent == null) 0
      else parent.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[SyncPDSSolver[Stmt, Fact, Field, W]#AddEpsilonToInitialStateListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (parent == null) if (other.parent != null) return false
      else if (!parent.equals(other.parent)) return false
      true
    }

    private def getOuterType = thisSyncPDSSolver
  }

  private class OnOutTransitionAddToStateListener(state: Nothing, private var nestedT: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      val returningNode = new Nothing(t.getLabel, nestedT.getStart.fact)
      setCallingContextReachable(returningNode)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (nestedT == null) 0
      else nestedT.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[SyncPDSSolver[Stmt, Fact, Field, W]#OnOutTransitionAddToStateListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (nestedT == null) if (other.nestedT != null) return false
      else if (!nestedT.equals(other.nestedT)) return false
      true
    }

    private def getOuterType = thisSyncPDSSolver
  }

  private class CallAutomatonListener extends Nothing {
    @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      if (!t.getStart.isInstanceOf[Nothing] && !t.getLabel.equals(callAutomaton.epsilon)) {
        val node = new Nothing(t.getLabel, t.getStart.fact)
        setCallingContextReachable(node)
      }
    }
  }

  def solve(curr: Nothing, field: Field, fieldTarget: Nothing, stmt: Stmt, callTarget: Nothing, weight: W): Unit = {
    fieldAutomaton.addInitialState(fieldTarget)
    callAutomaton.addInitialState(callTarget)
    val start = asFieldFact(curr)
    if (!field.equals(emptyField)) {
      val generateFieldState = generateFieldState(start, field)
      val fieldTrans = new Nothing(start, field, generateFieldState)
      fieldAutomaton.addTransition(fieldTrans)
      val fieldTransToInitial = new Nothing(generateFieldState, emptyField, fieldTarget)
      fieldAutomaton.addTransition(fieldTransToInitial)
    }
    else {
      val fieldTrans = new Nothing(start, emptyField, fieldTarget)
      fieldAutomaton.addTransition(fieldTrans)
    }
    val callTrans = new Nothing(wrap(curr.fact), curr.stmt, callTarget)
    callAutomaton.addWeightForTransition(callTrans, weight)
    processNode(curr)
  }

  def solve(curr: Nothing, field: Field, fieldTarget: Nothing, stmt: Stmt, callTarget: Nothing): Unit = {
    solve(curr, field, fieldTarget, stmt, callTarget, getCallWeights.getOne)
  }

  def processNode(curr: Nothing): Unit = {
    if (!addReachableState(curr)) return
    computeSuccessor(curr)
  }

  def propagate(curr: Nothing, s: Nothing): Unit = {
    if (s.isInstanceOf[Nothing]) {
      val succ = s.asInstanceOf[Nothing]
      if (succ.isInstanceOf[Nothing]) {
        val pushNode = succ.asInstanceOf[Nothing]
        val system = pushNode.system
        val location = pushNode.location
        processPush(curr, location, pushNode, system)
      }
      else processNormal(curr, succ)
    }
    else if (s.isInstanceOf[Nothing]) {
      val popNode = s.asInstanceOf[Nothing]
      processPop(curr, popNode)
    }
  }

  private def addReachableState(curr: Nothing): Boolean = {
    if (reachedStates.contains(curr)) return false
    reachedStates.add(curr)
    import scala.collection.JavaConversions._
    for (l <- Lists.newLinkedList(updateListeners)) {
      l.onReachableNodeAdded(curr)
    }
    import scala.collection.JavaConversions._
    for (l <- Lists.newLinkedList(reachedStateUpdateListeners.get(curr))) {
      l.reachable
    }
    true
  }

  def processNormal(curr: Nothing, succ: Nothing): Unit = {
    addNormalFieldFlow(curr, succ)
    addNormalCallFlow(curr, succ)
  }

  def addNormalCallFlow(curr: Nothing, succ: Nothing): Unit = {
    addCallRule(new Nothing(wrap(curr.fact), curr.stmt, wrap(succ.fact), succ.stmt, getCallWeights.normal(curr, succ)))
  }

  def addNormalFieldFlow(curr: Nothing, succ: Nothing): Unit = {
    if (succ.isInstanceOf[Nothing]) {
      val exNode = succ.asInstanceOf[Nothing]
      addFieldRule(new Nothing(asFieldFact(curr), fieldWildCard, asFieldFact(succ), exclusionFieldWildCard(exNode.exclusion), getFieldWeights.normal(curr, succ)))
      return
    }
    addFieldRule(new Nothing(asFieldFact(curr), fieldWildCard, asFieldFact(succ), fieldWildCard, getFieldWeights.normal(curr, succ)))
  }

  def asFieldFact(node: Nothing) = new Nothing(new Nothing(node.stmt, node.fact))

  def processPop(curr: Nothing, popNode: Nothing): Unit = {
    val system = popNode.system
    val location = popNode.location
    if (system.equals(SyncPDSSolver.PDSSystem.FIELDS)) {
      val node = location.asInstanceOf[Nothing]
      if (SyncPDSSolver.FieldSensitive) addFieldRule(new Nothing(asFieldFact(curr), node.location, asFieldFact(node.fact), getFieldWeights.pop(curr)))
      else addNormalFieldFlow(curr, node.fact)
      addNormalCallFlow(curr, node.fact)
    }
    else if (system.equals(SyncPDSSolver.PDSSystem.CALLS)) if (SyncPDSSolver.ContextSensitive) addCallRule(new Nothing(wrap(curr.fact), curr.stmt, wrap(location.asInstanceOf[Fact]), getCallWeights.pop(curr)))
  }

  private def applyCallSummary(callSite: Stmt, factInCallee: Fact, spInCallee: Stmt): Unit = {
    callAutomaton.addSummaryListener((t) => {
      val genSt = t.getTarget.asInstanceOf[Nothing]
      val sp = genSt.location
      val v = genSt.node.fact
      val exitStmt = t.getLabel
      val returnedFact = t.getStart.fact
      if (spInCallee.equals(sp) && factInCallee.equals(v)) {
        if (summaries.add(new SyncPDSSolver[Stmt, Fact, Field, W]#Summary(callSite, factInCallee, spInCallee, exitStmt, returnedFact))) {
          import scala.collection.JavaConversions._
          for (s <- Lists.newArrayList(summaryListeners)) {
            s.apply(callSite, factInCallee, spInCallee, exitStmt, returnedFact)
          }
        }
        // TODO can be removed and
        applyCallSummary(callSite, factInCallee, spInCallee, exitStmt, returnedFact)
      }
    })
  }

  private[solver] val summaries = Sets.newHashSet
  private[solver] val summaryListeners = Sets.newHashSet

  def addApplySummaryListener(l: SyncPDSSolver.OnAddedSummaryListener[_, _]): Unit = {
    if (summaryListeners.add(l)) {
      import scala.collection.JavaConversions._
      for (s <- Lists.newArrayList(summaries)) {
        l.apply(s.callSite, s.factInCallee, s.spInCallee, s.exitStmt, s.returnedFact)
      }
    }
  }

  private class Summary private(private val callSite: Stmt, private val factInCallee: Fact, private val spInCallee: Stmt, private val exitStmt: Stmt, private val returnedFact: Fact) {
    @Override def equals(o: Nothing): Boolean = {
      if (this eq o) return true
      if (o == null || (getClass ne o.getClass)) return false
      val summary = o.asInstanceOf[SyncPDSSolver[Stmt, Fact, Field, W]#Summary]
      Objects.equal(callSite, summary.callSite) && Objects.equal(factInCallee, summary.factInCallee) && Objects.equal(spInCallee, summary.spInCallee) && Objects.equal(exitStmt, summary.exitStmt) && Objects.equal(returnedFact, summary.returnedFact)
    }

    @Override def hashCode: Int = Objects.hashCode(callSite, factInCallee, spInCallee, exitStmt, returnedFact)
  }

  def applyCallSummary(callSite: Stmt, factInCallee: Fact, spInCallee: Stmt, exitStmt: Stmt, returnedFact: Fact): Unit

  def processPush(curr: Nothing, location: Nothing, succ: Nothing, system: SyncPDSSolver.PDSSystem): Unit = {
    if (system.equals(SyncPDSSolver.PDSSystem.FIELDS)) {
      if (SyncPDSSolver.FieldSensitive) addFieldRule(new Nothing(asFieldFact(curr), fieldWildCard, asFieldFact(succ), location.asInstanceOf[Field], fieldWildCard, getFieldWeights.push(curr, succ, location.asInstanceOf[Field])))
      else addNormalFieldFlow(curr, succ)
      addNormalCallFlow(curr, succ)
    }
    else if (system.equals(SyncPDSSolver.PDSSystem.CALLS)) {
      addNormalFieldFlow(curr, succ)
      if (SyncPDSSolver.ContextSensitive) addCallRule(new Nothing(wrap(curr.fact), curr.stmt, wrap(succ.fact), succ.stmt, location.asInstanceOf[Stmt], getCallWeights.push(curr, succ, location.asInstanceOf[Stmt])))
      else addNormalCallFlow(curr, succ)
      applyCallSummary(location.asInstanceOf[Stmt], succ.fact, succ.stmt)
    }
  }

  def addCallRule(rule: Nothing): Unit = {
    callingPDS.addRule(rule)
  }

  def addFieldRule(rule: Nothing): Unit = {
    fieldPDS.addRule(rule)
  }

  def getFieldWeights: Nothing

  def getCallWeights: Nothing

  private class FieldUpdateListener extends Nothing {
    @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      val n = t.getStart
      if (!n.isInstanceOf[Nothing] && !t.getLabel.equals(fieldAutomaton.epsilon)) {
        val fact = n.fact
        val node = new Nothing(fact.stmt, fact.fact)
        setFieldContextReachable(node)
      }
    }
  }

  private def setCallingContextReachable(node: Nothing): Unit = {
    if (!callingContextReachable.add(node)) return
    if (fieldContextReachable.contains(node)) processNode(node)
  }

  private def setFieldContextReachable(node: Nothing): Unit = {
    if (!fieldContextReachable.add(node)) return
    if (callingContextReachable.contains(node)) processNode(node)
  }

  def registerListener(listener: Nothing): Unit = {
    if (!updateListeners.add(listener)) return
    import scala.collection.JavaConversions._
    for (reachableNode <- Lists.newArrayList(reachedStates)) {
      listener.onReachableNodeAdded(reachableNode)
    }
  }

  def registerListener(listener: Nothing): Unit = {
    if (!reachedStateUpdateListeners.put(listener.getNode, listener)) return
    if (reachedStates.contains(listener.getNode)) listener.reachable
  }

  protected def wrap(variable: Fact) = new Nothing(variable)

  protected var generatedCallState: Nothing = Maps.newHashMap

  def generateCallState(d: Nothing, loc: Stmt): Nothing = {
    val e = new Nothing(d, loc)
    if (!generatedCallState.containsKey(e)) generatedCallState.put(e, new Nothing(d, loc))
    generatedCallState.get(e)
  }

  private[solver] val generatedFieldState = Maps.newHashMap

  def generateFieldState(d: Nothing, loc: Field): Nothing = {
    val e = new Nothing(d, loc)
    if (!generatedFieldState.containsKey(e)) generatedFieldState.put(e, new Nothing(d, loc))
    generatedFieldState.get(e)
  }

  def addGeneratedFieldState(state: Nothing): Unit = {
    val e = new Nothing(state.node, state.location)
    generatedFieldState.put(e, state)
  }

  def computeSuccessor(node: Nothing): Unit

  def epsilonField: Field

  def emptyField: Field

  def epsilonStmt: Stmt

  def exclusionFieldWildCard(exclusion: Field): Field

  def fieldWildCard: Field

  def getReachedStates: Nothing = Sets.newHashSet(reachedStates)

  def debugOutput(): Unit = {
    SyncPDSSolver.logger.debug(this.getClass.toString)
    SyncPDSSolver.logger.debug("All reachable states")
    prettyPrintSet(getReachedStates)
    val notFieldReachable = Sets.newHashSet(callingContextReachable)
    notFieldReachable.removeAll(getReachedStates)
    val notCallingContextReachable = Sets.newHashSet(fieldContextReachable)
    notCallingContextReachable.removeAll(getReachedStates)
    if (!notFieldReachable.isEmpty) {
      SyncPDSSolver.logger.debug("Calling context reachable")
      prettyPrintSet(notFieldReachable)
    }
    if (!notCallingContextReachable.isEmpty) {
      SyncPDSSolver.logger.debug("Field matching reachable")
      prettyPrintSet(notCallingContextReachable)
    }
    SyncPDSSolver.logger.debug(fieldPDS.toString)
    SyncPDSSolver.logger.debug(fieldAutomaton.toDotString)
    SyncPDSSolver.logger.debug(callingPDS.toString)
    SyncPDSSolver.logger.debug(callAutomaton.toDotString)
    SyncPDSSolver.logger.debug("===== end === " + this.getClass)
  }

  private def prettyPrintSet(set: Nothing): Unit = {
    var j = 0
    var s = ""
    import scala.collection.JavaConversions._
    for (reachableState <- set) {
      s += reachableState
      s += "\t"
      if ( {
        j += 1; j - 1
      } > 5) {
        s += "\n"
        j = 0
      }
    }
    SyncPDSSolver.logger.debug(s)
  }
}