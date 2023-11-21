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
package wpds.impl

import com.google.common.base.Joiner
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
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import pathexpression.Edge
import pathexpression.IRegEx
import pathexpression.LabeledGraph
import pathexpression.PathExpressionComputer
import pathexpression.RegEx
import wpds.interfaces.ForwardDFSEpsilonVisitor
import wpds.interfaces.ForwardDFSVisitor
import wpds.interfaces.Location
import wpds.interfaces.ReachabilityListener
import wpds.interfaces.State
import wpds.interfaces.WPAStateListener
import wpds.interfaces.WPAUpdateListener

object WeightedPAutomaton {
  private val LOGGER = LoggerFactory.getLogger(classOf[WeightedPAutomaton[_ <: Nothing, _ <: Nothing, _ <: Nothing]])
  private val SUMMARIZE = false
  private var count = 0

  trait SummaryListener[N <: Location, D <: State] {
    def addedSummary(t: Nothing): Unit
  }
}

abstract class WeightedPAutomaton[N <: Location, D <: State, W <: Weight] extends Nothing {
  private val transitionToWeights = new Nothing
  // Set Q is implicit
  // Weighted Pushdown Systems and their Application to Interprocedural
  // Dataflow Analysis
  protected var transitions: Nothing = Sets.newHashSet
  // set F in paper [Reps2003]
  protected var finalState: Nothing = Sets.newHashSet
  protected var initialStatesToSource: Nothing = HashMultimap.create
  // set P in paper [Reps2003]
  protected var states: Nothing = Sets.newHashSet
  final private val transitionsOutOf = HashMultimap.create
  final private val transitionsInto = HashMultimap.create
  private val listeners = Sets.newHashSet
  private val stateListeners = HashMultimap.create
  private val stateToDFS = Maps.newHashMap
  private val stateToEpsilonDFS = Maps.newHashMap
  private val nestedAutomatons = Sets.newHashSet
  private val nestedAutomataListeners = Sets.newHashSet
  private val stateToEpsilonReachabilityListener = Maps.newHashMap
  private val stateToReachabilityListener = Maps.newHashMap
  private val connectedPushes = Sets.newHashSet
  private val conntectedPushListeners = Sets.newHashSet
  private val unbalancedPopListeners = Sets.newHashSet
  private val unbalancedPops = Maps.newHashMap
  private val transitionsToFinalWeights = Maps.newHashMap
  private var dfsVisitor: Nothing = null
  private var dfsEpsVisitor: Nothing = null
  var failedAdditions = 0
  var failedDirectAdditions = 0
  private var initialAutomaton: WeightedPAutomaton[N, D, W] = null
  private var pathExpressionComputer: Nothing = null
  private var lastStates = 0
  private val watch = Stopwatch.createUnstarted
  private val stateToDistanceToInitial = Maps.newHashMap
  private val stateToUnbalancedDistance = Maps.newHashMap
  final private val stateCreatingTransition = Maps.newHashMap

  def createState(d: D, loc: N): D

  def isGeneratedState(d: D): Boolean

  def getTransitions: Nothing = Lists.newArrayList(transitions)

  def addTransition(trans: Nothing): Boolean = {
    val addWeightForTransition = addWeightForTransition(trans, getOne)
    if (!addWeightForTransition) failedDirectAdditions += 1
    addWeightForTransition
  }

  def getFinalState: Nothing = finalState

  def toString: Nothing = {
    var s = "PAutomaton\n"
    s += "\tInitialStates:" + initialStatesToSource.keySet + "\n"
    s += "\tFinalStates:" + finalState + "\n"
    s += "\tWeightToTransitions:\n\t\t"
    s += Joiner.on("\n\t\t").join(transitionToWeights.entrySet)
    import scala.collection.JavaConversions._
    for (nested <- nestedAutomatons) {
      s += "\n"
      s += nested.toString
    }
    s
  }

  private def wrapIfInitialOrFinalState(s: D) = if (initialStatesToSource.containsKey(s)) "ENTRY: " + wrapFinalState(s)
  else wrapFinalState(s)

  private def wrapFinalState(s: D) = if (finalState.contains(s)) "TO: " + s + ""
  else s.toString

  def toDotString: Nothing = toDotString(Sets.newHashSet[WeightedPAutomaton[N, D, W]])

  private def toDotString(visited: Nothing): Nothing = {
    if (!visited.add(this)) return "NESTED loop: " + getInitialStates
    var s = "digraph {\n"
    val trans = new Nothing
    val summaryIdentifier = Lists.newArrayList
    val removableTrans = Sets.newHashSet
    if (WeightedPAutomaton.SUMMARIZE) {
      val mergableStates = HashBasedTable.create
      import scala.collection.JavaConversions._
      for (source <- states) {
        if (transitionsInto.get(source).isEmpty && (transitionsOutOf.get(source).size eq 1)) {
          import scala.collection.JavaConversions._
          for (t <- transitionsOutOf.get(source)) {
            var set = mergableStates.get(t.getLabel, t.getTarget)
            if (set == null) set = Sets.newHashSet
            set.add(t)
            removableTrans.add(t)
            mergableStates.put(t.getLabel, t.getTarget, set)
          }
        }
      }
      import scala.collection.JavaConversions._
      for (label <- mergableStates.rowKeySet) {
        import scala.collection.JavaConversions._
        for (target <- mergableStates.columnKeySet) {
          val trs = mergableStates.get(label, target)
          if (trs == null) continue //todo: continue is not supported
          val labels = Lists.newLinkedList
          import scala.collection.JavaConversions._
          for (t <- trs) {
            labels.add(escapeQuotes(wrapIfInitialOrFinalState(t.getStart)))
          }
          if (!labels.isEmpty) {
            summaryIdentifier.add(Joiner.on("\\n").join(labels))
            var v = "\t\"" + "SUMNODE_" + summaryIdentifier.size + "\""
            v += " -> \"" + escapeQuotes(wrapIfInitialOrFinalState(target)) + "\""
            v += "[label=\"" + escapeQuotes(label.toString) + "\"];\n"
            trans.add(v)
          }
        }
      }
    }
    import scala.collection.JavaConversions._
    for (source <- states) {
      val collection = transitionsOutOf.get(source)
      import scala.collection.JavaConversions._
      for (target <- states) {
        val labels = Lists.newLinkedList
        import scala.collection.JavaConversions._
        for (t <- collection) {
          if (removableTrans.contains(t)) continue //todo: continue is not supported
          if (t.getTarget.equals(target)) labels.add(escapeQuotes(t.getLabel.toString) + " W: " + transitionToWeights.get(t))
        }
        if (!labels.isEmpty) {
          var v = "\t\"" + escapeQuotes(wrapIfInitialOrFinalState(source)) + "\""
          v += " -> \"" + escapeQuotes(wrapIfInitialOrFinalState(target)) + "\""
          v += "[label=\"" + Joiner.on("\\n").join(labels) + "\"];\n"
          trans.add(v)
        }
      }
    }
    s += Joiner.on("").join(trans)
    s += "}\n"
    if (WeightedPAutomaton.SUMMARIZE) {
      val i = 1
      import scala.collection.JavaConversions._
      for (node <- summaryIdentifier) {
        s += "SUMNODE_" + i + ":\n"
        s += node
        s += "\n"
      }
    }
    s += "Transitions: " + transitions.size + " Nested: " + nestedAutomatons.size + "\n"
    import scala.collection.JavaConversions._
    for (nested <- nestedAutomatons) {
      s += "NESTED -> \n"
      s += nested.toDotString(visited)
    }
    s += "End nesting\n"
    s
  }

  def getInitialStates: Nothing = initialStatesToSource.keySet

  private def escapeQuotes(string: Nothing) = string.replace("\"", "")

  def toLabelGroupedDotString: Nothing = {
    val groupedByTargetAndLabel = HashBasedTable.create
    import scala.collection.JavaConversions._
    for (t <- transitions) {
      var collection = groupedByTargetAndLabel.get(t.getTarget, t.getLabel)
      if (collection == null) collection = Sets.newHashSet
      collection.add(t.getStart)
      groupedByTargetAndLabel.put(t.getTarget, t.getLabel, collection)
    }
    var s = "digraph {\n"
    import scala.collection.JavaConversions._
    for (target <- groupedByTargetAndLabel.rowKeySet) {
      import scala.collection.JavaConversions._
      for (label <- groupedByTargetAndLabel.columnKeySet) {
        val source = groupedByTargetAndLabel.get(target, label)
        if (source == null) continue //todo: continue is not supported
        s += "\t\"" + Joiner.on("\\n").join(source) + "\""
        s += " -> \"" + wrapIfInitialOrFinalState(target) + "\""
        s += "[label=\"" + label + "\"];\n"
      }
    }
    s += "}\n"
    s += "Transitions: " + transitions.size + "\n"
    import scala.collection.JavaConversions._
    for (nested <- nestedAutomatons) {
      s += "NESTED -> \n"
      s += nested.toDotString
    }
    s
  }

  def epsilon: N

  def extractLanguage(from: D): Nothing = {
    val expr = new Nothing(this)
    var res: Nothing = null
    import scala.collection.JavaConversions._
    for (finalState <- getFinalState) {
      val regEx = expr.getExpressionBetween(from, finalState)
      if (res == null) res = regEx
      else res = RegEx.union[N](res, regEx)
    }
    if (res == null) return new Nothing
    res
  }

  def extractLanguage(from: D, to: D): Nothing = {
    val expr = new Nothing(this)
    val res = expr.getExpressionBetween(from, to)
    if (res == null) return new Nothing
    res
  }

  def getStates: Nothing = states

  def getEdges: Nothing = {
    val trans = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (tran <- transitions) {
      if (!tran.getLabel.equals(epsilon)) trans.add(new Nothing(tran.getTarget, tran.getLabel, tran.getStart))
    }
    trans
  }

  def getNodes: Nothing = getStates

  def addWeightForTransition(trans: Nothing, weight: W): Boolean = {
    if (weight == null) throw new Nothing("Weight must not be null!")
    if (trans.getStart.equals(trans.getTarget) && trans.getLabel.equals(epsilon)) {
      failedAdditions += 1
      return false
    }
    val distanceToInitial = computeDistance(trans)
    if (hasMaxDepth && distanceToInitial > getMaxDepth) return false
    if (!watch.isRunning) watch.start
    transitionsOutOf.get(trans.getStart).add(trans)
    transitionsInto.get(trans.getTarget).add(trans)
    if (states.add(trans.getTarget)) stateCreatingTransition.put(trans.getTarget, trans)
    states.add(trans.getStart)
    var added = transitions.add(trans)
    val oldWeight = transitionToWeights.get(trans)
    val newWeight = (if (oldWeight == null) weight
    else oldWeight.combineWith(weight)).asInstanceOf[W]
    if (!newWeight.equals(oldWeight)) {
      transitionToWeights.put(trans, newWeight)
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(listeners)) {
        l.onWeightAdded(trans, newWeight, this)
      }
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(stateListeners.get(trans.getStart))) {
        l.onOutTransitionAdded(trans, newWeight, this)
      }
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(stateListeners.get(trans.getTarget))) {
        l.onInTransitionAdded(trans, newWeight, this)
      }
      added = true
    }
    if (watch.isRunning) watch.stop
    if (!added) failedAdditions += 1
    added
  }

  protected def computeDistance(trans: Nothing): Int = {
    var distance: Nothing = null
    if (isUnbalancedState(trans.getTarget)) distance = 0
    else {
      distance = stateToDistanceToInitial.get(trans.getTarget)
      if (distance == null) return -1
    }
    val integer = {
      distance += 1; distance
    }
    val currDistance = stateToDistanceToInitial.get(trans.getStart)
    if (currDistance == null || integer < currDistance) {
      stateToDistanceToInitial.put(trans.getStart, integer)
      return integer
    }
    currDistance
  }

  def getWeightFor(trans: Nothing): W = transitionToWeights.get(trans)

  def registerListener(listener: Nothing): Unit = {
    if (!listeners.add(listener)) return
    import scala.collection.JavaConversions._
    for (transAndWeight <- Lists.newArrayList(transitionToWeights.entrySet)) {
      listener.onWeightAdded(transAndWeight.getKey, transAndWeight.getValue, this)
    }
    import scala.collection.JavaConversions._
    for (nested <- Lists.newArrayList(nestedAutomatons)) {
      nested.registerListener(listener)
    }
  }

  private def increaseListenerCount(l: Nothing): Unit = {
    WeightedPAutomaton.count += 1
    if (WeightedPAutomaton.count % 100000 == 0) onManyStateListenerRegister()
  }

  def onManyStateListenerRegister(): Unit = {
  }

  def registerListener(l: Nothing): Unit = {
    if (!stateListeners.put(l.getState, l)) return
    increaseListenerCount(l)
    import scala.collection.JavaConversions._
    for (t <- Lists.newArrayList(transitionsOutOf.get(l.getState))) {
      l.onOutTransitionAdded(t, transitionToWeights.get(t), this)
    }
    import scala.collection.JavaConversions._
    for (t <- Lists.newArrayList(transitionsInto.get(l.getState))) {
      l.onInTransitionAdded(t, transitionToWeights.get(t), this)
    }
    import scala.collection.JavaConversions._
    for (nested <- Lists.newArrayList(nestedAutomatons)) {
      nested.registerListener(l)
    }
  }

  def addFinalState(state: D): Unit = {
    this.finalState.add(state)
  }

  def registerDFSListener(state: D, l: Nothing): Unit = {
    stateToReachabilityListener.put(state, l)
    if (dfsVisitor == null) {
      dfsVisitor = new Nothing(this)
      this.registerListener(dfsVisitor)
    }
    dfsVisitor.registerListener(state, l)
  }

  protected def getStateToDFS: Nothing = stateToDFS

  def registerDFSEpsilonListener(state: D, l: Nothing): Unit = {
    stateToEpsilonReachabilityListener.put(state, l)
    if (dfsEpsVisitor == null) {
      dfsEpsVisitor = new Nothing(this)
      this.registerListener(dfsEpsVisitor)
    }
    import scala.collection.JavaConversions._
    for (nested <- Lists.newLinkedList(nestedAutomatons)) {
      nested.registerDFSEpsilonListener(state, l)
    }
    dfsEpsVisitor.registerListener(state, l)
  }

  protected def getStateToEpsilonDFS: Nothing = stateToEpsilonDFS

  def getOne: W

  def createNestedAutomaton(initialState: D): WeightedPAutomaton[N, D, W] = {
    val nested = new WeightedPAutomaton[N, D, W]() {
      @Override override def createState(d: D, loc: N): D = thisWeightedPAutomaton.createState(d, loc)

      @Override override def epsilon: N = thisWeightedPAutomaton.epsilon

      @Override override def getOne: W = thisWeightedPAutomaton.getOne

      @Override override def isGeneratedState(d: D): Boolean = thisWeightedPAutomaton.isGeneratedState(d)

      @Override override protected def getStateToDFS: Nothing = thisWeightedPAutomaton.stateToDFS

      @Override override protected def getStateToEpsilonDFS: Nothing = thisWeightedPAutomaton.stateToEpsilonDFS

      @Override override def nested = true

      @Override override def toString: Nothing = "NESTED: \n" + super.toString
    }
    addNestedAutomaton(nested)
    nested
  }

  def registerUnbalancedPopListener(l: Nothing): Unit = {
    if (unbalancedPopListeners.add(l)) {
      import scala.collection.JavaConversions._
      for (e <- Lists.newArrayList(unbalancedPops.entrySet)) {
        val t = e.getKey
        l.unbalancedPop(t.targetState, t.trans, e.getValue)
      }
    }
  }

  def unbalancedPop(targetState: D, trans: Nothing, weight: W): Unit = {
    val t = new WeightedPAutomaton[N, D, W]#UnbalancedPopEntry(targetState, trans)
    val oldVal = unbalancedPops.get(t)
    val newVal = if (oldVal == null) weight
    else oldVal.combineWith(weight).asInstanceOf[W]
    if (!newVal.equals(oldVal)) {
      unbalancedPops.put(t, newVal)
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(unbalancedPopListeners)) {
        l.unbalancedPop(targetState, trans, newVal)
      }
    }
  }

  private val summaryEdges = Sets.newHashSet
  private val summaryEdgeListener = Sets.newHashSet

  def registerSummaryEdge(t: Nothing): Unit = {
    if (summaryEdges.add(t)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(summaryEdgeListener)) {
        l.addedSummary(t)
      }
    }
  }

  def addSummaryListener(l: WeightedPAutomaton.SummaryListener[N, D]): Unit = {
    if (summaryEdgeListener.add(l)) {
      import scala.collection.JavaConversions._
      for (edge <- Lists.newArrayList(summaryEdges)) {
        l.addedSummary(edge)
      }
      import scala.collection.JavaConversions._
      for (nested <- Lists.newArrayList(nestedAutomatons)) {
        nested.addSummaryListener(l)
      }
    }
  }

  private class UnbalancedPopEntry(private val targetState: D, private val trans: Nothing) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (targetState == null) 0
      else targetState.hashCode)
      result = prime * result + (if (trans == null) 0
      else trans.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedPAutomaton[N, D, W]#UnbalancedPopEntry]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (targetState == null) if (other.targetState != null) return false
      else if (!targetState.equals(other.targetState)) return false
      if (trans == null) if (other.trans != null) return false
      else if (!trans.equals(other.trans)) return false
      true
    }

    private def getOuterType = thisWeightedPAutomaton
  }

  private class ReturnSiteWithWeights(private val returnSite: N, private val returnedFact: D, private val returnedWeight: W) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (returnSite == null) 0
      else returnSite.hashCode)
      result = prime * result + (if (returnedFact == null) 0
      else returnedFact.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedPAutomaton[N, D, W]#ReturnSiteWithWeights]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (returnSite == null) if (other.returnSite != null) return false
      else if (!returnSite.equals(other.returnSite)) return false
      if (returnedFact == null) if (other.returnedFact != null) return false
      else if (!returnedFact.equals(other.returnedFact)) return false
      if (returnedWeight == null) if (other.returnedWeight != null) return false
      else if (!returnedWeight.equals(other.returnedWeight)) return false
      true
    }

    private def getOuterType = thisWeightedPAutomaton
  }

  def getTransitionsToFinalWeights: Nothing = {
    WeightedPAutomaton.LOGGER.trace("Start computing final weights")
    val w = Stopwatch.createStarted
    import scala.collection.JavaConversions._
    for (s <- initialStatesToSource.keySet) {
      registerListener(new WeightedPAutomaton[N, D, W]#ValueComputationListener(s, getOne))
    }
    WeightedPAutomaton.LOGGER.trace("Finished computing final weights in {}", w)
    transitionsToFinalWeights
  }

  private class ValueComputationListener(state: D, private var weight: W) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      val newWeight = weight.extendWith(w).asInstanceOf[W]
      val weightAtTarget = transitionsToFinalWeights.get(t)
      val newVal = if (weightAtTarget == null) newWeight
      else weightAtTarget.combineWith(newWeight).asInstanceOf[W]
      transitionsToFinalWeights.put(t, newVal)
      if (isGeneratedState(t.getStart)) registerListener(new WeightedPAutomaton[N, D, W]#ValueComputationListener(t.getStart, newVal))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (weight == null) 0
      else weight.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[WeightedPAutomaton[N, D, W]#ValueComputationListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (weight == null) if (other.weight != null) return false
      else if (!weight.equals(other.weight)) return false
      true
    }

    private def getOuterType = thisWeightedPAutomaton
  }

  def nested = false

  def addNestedAutomaton(nested: WeightedPAutomaton[N, D, W]): Unit = {
    if (!nestedAutomatons.add(nested)) return
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(stateListeners.values)) {
      nested.registerListener(e)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(listeners)) {
      nested.registerListener(e)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(summaryEdgeListener)) {
      nested.addSummaryListener(e)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(unbalancedPopListeners)) {
      nested.registerUnbalancedPopListener(e)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(stateToEpsilonReachabilityListener.entrySet)) {
      nested.registerDFSEpsilonListener(e.getKey, e.getValue)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(stateToReachabilityListener.entrySet)) {
      nested.registerDFSListener(e.getKey, e.getValue)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(stateToReachabilityListener.entrySet)) {
      nested.registerDFSListener(e.getKey, e.getValue)
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(nestedAutomataListeners)) {
      e.nestedAutomaton(this, nested)
      nested.registerNestedAutomatonListener(e)
    }
  }

  def registerNestedAutomatonListener(l: Nothing): Unit = {
    if (!nestedAutomataListeners.add(l)) return
    import scala.collection.JavaConversions._
    for (nested <- Lists.newArrayList(nestedAutomatons)) {
      l.nestedAutomaton(this, nested)
    }
  }

  def setInitialAutomaton(aut: WeightedPAutomaton[N, D, W]): Unit = {
    initialAutomaton = aut
  }

  def isInitialAutomaton(aut: WeightedPAutomaton[N, D, W]): Boolean = initialAutomaton.equals(aut)

  def toRegEx(start: D, end: D): Nothing = {
    if (lastStates < states.size) {
      pathExpressionComputer = new Nothing(this)
      lastStates = states.size
    }
    RegEx.reverse(pathExpressionComputer.getExpressionBetween(end, start))
  }

  def containsLoop: Boolean = {
    // Performs a backward DFS
    val visited = Sets.newHashSet
    val worklist = Lists.newLinkedList
    worklist.addAll(initialStatesToSource.keySet)
    while (!worklist.isEmpty) {
      val pop = worklist.pop
      visited.add(pop)
      val inTrans = transitionsInto.get(pop)
      import scala.collection.JavaConversions._
      for (t <- inTrans) {
        if (t.getLabel.equals(this.epsilon)) continue //todo: continue is not supported
        if (!isGeneratedState(t.getStart)) continue //todo: continue is not supported
        if (visited.contains(t.getStart)) return true
        worklist.add(t.getStart)
      }
    }
    false
  }

  def getLongestPath: Nothing = {
    // Performs a backward DFS
    val worklist = Lists.newLinkedList
    worklist.addAll(initialStatesToSource.keySet)
    val pathReachingD = Maps.newHashMap
    while (!worklist.isEmpty) {
      val pop = worklist.pop
      val atCurr = getOrCreate(pathReachingD, pop)
      val inTrans = transitionsInto.get(pop)
      import scala.collection.JavaConversions._
      for (t <- inTrans) {
        if (t.getLabel.equals(this.epsilon)) continue //todo: continue is not supported
        val next = t.getStart
        if (!isGeneratedState(next)) continue //todo: continue is not supported
        if (next.equals(pop)) continue //todo: continue is not supported
        val atNext = getOrCreate(pathReachingD, next)
        val newAtCurr = Sets.newHashSet(atCurr)
        if (newAtCurr.add(t.getLabel)) {
          val addAll = atNext.addAll(newAtCurr)
          if (addAll) worklist.add(next)
        }
      }
    }
    var longest = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (l <- pathReachingD.values) {
      if (longest.size < l.size) longest = l
    }
    longest
  }

  private def getOrCreate(pathReachingD: Nothing, pop: D) = {
    var collection = pathReachingD.get(pop)
    if (collection == null) {
      collection = Sets.newHashSet
      pathReachingD.put(pop, collection)
    }
    collection
  }

  def isUnbalancedState(target: D): Boolean = initialStatesToSource.containsKey(target)

  def addUnbalancedState(state: D, parent: D): Boolean = {
    var distance = 0
    val parents = Sets.newHashSet
    if (!initialStatesToSource.containsKey(parent)) {
      distance = stateToUnbalancedDistance.get(parent)
      parents.add(parent)
    }
    else parents.addAll(initialStatesToSource.get(parent))
    val newDistance = {
      distance += 1; distance
    }
    stateToUnbalancedDistance.put(state, newDistance)
    if (getMaxUnbalancedDepth > 0 && newDistance > getMaxUnbalancedDepth) return false
    initialStatesToSource.putAll(state, parents)
    true
  }

  def addInitialState(state: D): Boolean = initialStatesToSource.put(state, state)

  def unregisterAllListeners(): Unit = {
    this.conntectedPushListeners.clear
    this.nestedAutomataListeners.clear
    this.stateListeners.clear
    this.listeners.clear
    this.stateToEpsilonReachabilityListener.clear
    this.stateToReachabilityListener.clear
    this.summaryEdgeListener.clear
    this.unbalancedPopListeners.clear
  }

  def getWatch: Nothing = watch

  def hasMaxDepth: Boolean = getMaxDepth > 0

  def getMaxDepth: Int = -1

  def getMaxUnbalancedDepth: Int = -1

  def getUnbalancedStartOf(target: D): Nothing = initialStatesToSource.get(target)
}