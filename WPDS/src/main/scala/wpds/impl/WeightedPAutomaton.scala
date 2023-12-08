package wpds.impl

import com.google.common.base.{Joiner, Stopwatch}
import com.google.common.collect._
import org.slf4j.{Logger, LoggerFactory}
import pathexpression.{Edge, IRegEx, LabeledGraph, PathExpressionComputer, RegEx}
import wpds.interfaces._

import scala.collection.mutable.{Set, TreeSet, ListBuffer, HashMap, HashSet}
import com.google.common.collect.{Table, HashBasedTable}

import com.google.common.collect._
import org.slf4j.{Logger, LoggerFactory}
import pathexpression.LabeledGraph
import scala.collection.mutable
import scala.jdk.CollectionConverters._

abstract class WeightedPAutomaton[N <: Location, D <: State, W <: Weight] extends LabeledGraph[D, N] {
    

    private val LOGGER = LoggerFactory.getLogger(classOf[WeightedPAutomaton[N, D, W]])
    private val transitionToWeights = mutable.HashMap[Transition[N, D], W]()
    protected val transitions = mutable.HashSet[Transition[N, D]]()
    protected val finalState = mutable.HashSet[D]()
    protected val initialStatesToSource = mutable.HashMap[D, mutable.HashSet[D]]()
    protected val states = mutable.HashSet[D]()
    private val transitionsOutOf = mutable.HashMap[D, mutable.HashSet[Transition[N, D]]]()
    private val transitionsInto = mutable.HashMap[D, mutable.HashSet[Transition[N, D]]]()
    private val listeners = mutable.HashSet[WPAUpdateListener[N, D, W]]()
    private val stateListeners = mutable.HashMap[D, mutable.HashSet[WPAStateListener[N, D, W]]]()
    private val stateToDFS = mutable.HashMap[D, ForwardDFSVisitor[N, D, W]]()
    private val stateToEpsilonDFS = mutable.HashMap[D, ForwardDFSVisitor[N, D, W]]()
    private val nestedAutomatons = mutable.HashSet[WeightedPAutomaton[N, D, W]]()
    private val nestedAutomataListeners = mutable.HashSet[NestedAutomatonListener[N, D, W]]()
    private val stateToEpsilonReachabilityListener = mutable.HashMap[D, ReachabilityListener[N, D]]()
    private val stateToReachabilityListener = mutable.HashMap[D, ReachabilityListener[N, D]]()
    private val connectedPushes = mutable.HashSet[ReturnSiteWithWeights]()
    private val connectedPushListeners = mutable.HashSet[ConnectPushListener[N, D, W]]()
    private val unbalancedPopListeners = mutable.HashSet[UnbalancedPopListener[N, D, W]]()
    private val unbalancedPops = mutable.HashMap[UnbalancedPopEntry, W]()
    private val transitionsToFinalWeights = mutable.HashMap[Transition[N, D], W]()
    private var dfsVisitor: ForwardDFSVisitor[N, D, W] = _
    private var dfsEpsVisitor: ForwardDFSVisitor[N, D, W] = _
    var failedAdditions: Int = _
    var failedDirectAdditions: Int = _
    private var initialAutomaton: WeightedPAutomaton[N, D, W] = _
    private var pathExpressionComputer: PathExpressionComputer[D, N] = _
    private var lastStates: Int = _
    private val watch = Stopwatch.createUnstarted()
    private val stateToDistanceToInitial = mutable.HashMap[D, Integer]()
    private val stateToUnbalancedDistance = mutable.HashMap[D, Integer]()
    private val stateCreatingTransition = mutable.HashMap[D, Transition[N, D]]()
    def createState(d: D, loc: N): D

    def isGeneratedState(d: D): Boolean

    def getTransitions: Collection[Transition[N, D]] = {
        Lists.newArrayList(transitions)
    }

    def addTransition(trans: Transition[N, D]): Boolean = {
        val addWForTransition: Boolean = addWeightForTransition(trans, getOne)
        if (!addWForTransition) {
            failedDirectAdditions += 1
        }
        addWForTransition
    }

    def getFinalState: HashSet[D] = finalState

    override def toString: String = {
        var s = "PAutomaton\n"
        s += "\tInitialStates:" + initialStatesToSource.keySet + "\n"
        s += "\tFinalStates:" + finalState + "\n"
        s += "\tWeightToTransitions:\n\t\t"
        s += transitionToWeights.mkString("\n\t\t")

        for (nested <- nestedAutomatons) {
            s += "\n"
            s += nested.toString
        }
        s
    }

    private def wrapIfInitialOrFinalState(s: D): String = {
        if (initialStatesToSource.contains(s)) "ENTRY: " + wrapFinalState(s) else wrapFinalState(s)
    }

    private def wrapFinalState(s: D): String = {
        if (finalState.contains(s)) "TO: " + s else s.toString
    }

    private val SUMMARIZE = false

    def toDotString: String = toDotString(HashSet[WeightedPAutomaton[N, D, W]]())

    //private def toDotString(visited: Set[WeightedPAutomaton[N, D, W]]): String = {
      //  if (!visited.add(this)) "NESTED loop: " + getInitialStates else ""
    //}

    private def toDotString(visited: HashSet[WeightedPAutomaton[N, D, W]]): String = {
        if (!visited.add(this)) {
            return "NESTED loop: " + getInitialStates
        }
        var s = "digraph {\n"
        val trans = new TreeSet[String]()
        val summaryIdentifier = new ListBuffer[String]()
        val removableTrans = HashSet[Transition[N, D]]()
        if (SUMMARIZE) {
            val mergableStates = HashBasedTable.create[N, D, HashSet[Transition[N, D]]]()
            for (source <- states) {
            if (transitionsInto.get(source).isEmpty && transitionsOutOf.get(source).size == 1) {
                for (t <- transitionsOutOf.get(source).flatten) {
                    var set = mergableStates.get(t.getLabel, t.getTarget)
                    if (set == null) {
                        set = HashSet[Transition[N, D]]()
                }
                set.add(t)
                removableTrans.add(t)
                mergableStates.put(t.getLabel, t.getTarget, set)
                }
            }
            }
            for (label <- mergableStates.rowKeySet()) {
            for (target <- mergableStates.columnKeySet()) {
                val trs = mergableStates.get(label, target)
                if (trs != null) {
                val labels = new ListBuffer[String]()
                for (t <- trs) {
                    labels.add(escapeQuotes(wrapIfInitialOrFinalState(t.getStart)))
                }
                if (labels.nonEmpty) {
                    summaryIdentifier.add(labels.mkString("\\n"))
                    var v = "\t\"" + "SUMNODE_" + summaryIdentifier.size + "\""
                    v += " -> \"" + escapeQuotes(wrapIfInitialOrFinalState(target)) + "\""
                    v += "[label=\"" + escapeQuotes(label.toString) + "\"];\n"
                    trans.add(v)
                }
                }
            }
            }
        }
        for (source <- states) {
            val collection = transitionsOutOf.get(source)
            for (target <- states) {
            val labels = new ListBuffer[String]()
            for (t <- collection.flatten) {
                if (!removableTrans.contains(t) && t.getTarget.equals(target)) {
                labels += escapeQuotes(t.getLabel.toString) + " W: " + transitionToWeights.get(t)
                }
            }
            if (labels.nonEmpty) {
                var v = "\t\"" + escapeQuotes(wrapIfInitialOrFinalState(source)) + "\""
                v += " -> \"" + escapeQuotes(wrapIfInitialOrFinalState(target)) + "\""
                v += "[label=\"" + labels.mkString("\\n") + "\"];\n"
                trans.add(v)
            }
            }
        }
        s += trans.mkString("")
        s += "}\n"
        if (SUMMARIZE) {
            var i = 1
            for (node <- summaryIdentifier) {
            s += "SUMNODE_" + i + ":\n"
            s += node
            s += "\n"
            i += 1
            }
        }
        s += "Transitions: " + transitions.size + " Nested: " + nestedAutomatons.size + "\n"
        for (nested <- nestedAutomatons) {
            s += "NESTED -> \n"
            s += nested.toDotString(visited)
        }
        s += "End nesting\n"
        s
    }

    def getInitialStates: Set[D] = initialStatesToSource.keySet

    private def escapeQuotes(string: String): String = string.replace("\"", "")

    def toLabelGroupedDotString: String = {
        val groupedByTargetAndLabel = HashBasedTable.create[D, N, Collection[D]]()
        for (t <- transitions) {
            var collection = groupedByTargetAndLabel.get(t.getTarget, t.getLabel)
            if (collection == null) collection = Sets.newHashSet()
            collection.add(t.getStart)
            groupedByTargetAndLabel.put(t.getTarget, t.getLabel, collection)
        }
        var s = "digraph {\n"
        for (target <- groupedByTargetAndLabel.keys) {
            for (label <- groupedByTargetAndLabel.columnKeySet) {
                val source = groupedByTargetAndLabel.get(target, label)
                if (source != null) {
                    s += "\t\"" + Joiner.on("\\n").join(source) + "\""
                    s += " -> \"" + wrapIfInitialOrFinalState(target) + "\""
                    s += "[label=\"" + label + "\"];\n"
                }
            }
        }
        s += "}\n"
        s += "Transitions: " + transitions.size + "\n"
        for (nested <- nestedAutomatons) {
            s += "NESTED -> \n"
            s += nested.toDotString
        }
        s
    }

    def epsilon: N

    def extractLanguage(from: D): IRegEx[N] = {
        val expr = new PathExpressionComputer[D, N](this)
        var res: IRegEx[N] = null
        for (finalState <- getFinalState) {
            val regEx = expr.getExpressionBetween(from, finalState)
            if (res == null) {
                res = regEx
            } else {
                res = RegEx.union(res, regEx)
            }
        }
        if (res == null) new RegEx.EmptySet[N]() else res
    }

    def extractLanguage(from: D, to: D): IRegEx[N] = {
        val expr = new PathExpressionComputer[D, N](this)
        val res = expr.getExpressionBetween(from, to)
        if (res == null) new RegEx.EmptySet[N]() else res
    }


    def getStates: HashSet[D] = states

    def getEdges: HashSet[Edge[D, N]] = {
        val trans = HashSet[Edge[D, N]]()
        for (tran <- transitions) {
            if (!tran.getLabel.equals(epsilon)) {
                trans.add(new Transition[N, D](tran.getTarget, tran.getLabel, tran.getStart))
            }
        }
        trans
    }

    def getNodes: HashSet[D] = getStates
def addWeightForTransition(trans: Transition[N, D], weight: W): Boolean = {
    if (weight == null) throw new IllegalArgumentException("Weight must not be null!")
    if (trans.getStart.equals(trans.getTarget) && trans.getLabel.equals(epsilon)) {
        failedAdditions += 1
        false
    } else {
        val distanceToInitial = computeDistance(trans)
        if (hasMaxDepth && distanceToInitial > getMaxDepth) {
            false
        } else {
            if (!watch.isRunning) {
                watch.start()
            }
            transitionsOutOf(trans.getStart).add(trans)
            transitionsInto(trans.getTarget).add(trans)
            if (states.add(trans.getTarget)) {
                stateCreatingTransition.put(trans.getTarget, trans)
            }
            states.add(trans.getStart)
            var added = transitions.add(trans)
            val oldWeight = transitionToWeights(trans)
            val newWeight = if (oldWeight == null) weight else oldWeight.combineWith(weight).asInstanceOf[W]

            if (!newWeight.equals(oldWeight)) {
                transitionToWeights.put(trans, newWeight)

                for (l <- listeners.toList) {
                    l.onWeightAdded(trans, newWeight, this)
                }
                for (l <- stateListeners(trans.getStart).toList) {
                    l.onOutTransitionAdded(trans, newWeight, this)
                }
                for (l <- stateListeners(trans.getTarget).toList) {
                    l.onInTransitionAdded(trans, newWeight, this)
                }
                added = true
            }
            if (watch.isRunning) watch.stop()
            if (!added) failedAdditions += 1
            added
        }
    }
}

    protected def computeDistance(trans: Transition[N, D]): Int = {
        val distance = if (isUnbalancedState(trans.getTarget)) {
            0
        } else {
            val distance = stateToDistanceToInitial(trans.getTarget)
            if (distance == null) {
                -1
            } else {
                distance
            }
        }
        val integer = distance + 1
        val currDistance = stateToDistanceToInitial(trans.getStart)
        if (currDistance == null || integer < currDistance) {
            stateToDistanceToInitial.put(trans.getStart, integer)
            integer
        } else {
            currDistance
        }
    }

    def getWeightFor(trans: Transition[N, D]): W = transitionToWeights(trans)


    def registerListener(listener: WPAUpdateListener[N, D, W]): Unit = {
        if (!listeners.contains(listener)) {
            listeners += listener
            for ((trans, weight) <- transitionToWeights) {
                listener.onWeightAdded(trans, weight, this)
            }
            for (nested <- nestedAutomatons) {
                nested.registerListener(listener)
            }
        }
    }

    private var count = 0

    private def increaseListenerCount(l: WPAStateListener[N, D, W]): Unit = {
        count += 1
        if (count % 100000 == 0) {
            onManyStateListenerRegister()
        }
    }

    def onManyStateListenerRegister(): Unit = {}

    def registerListener(l: WPAStateListener[N, D, W]): Unit = {
        if (!stateListeners.contains(l.getState)) {
            stateListeners += (l.getState -> mutable.HashSet(l))
            increaseListenerCount(l)
            for (t <- transitionsOutOf.getOrElse(l.getState, List())) {
                l.onOutTransitionAdded(t, transitionToWeights(t), this)
            }
            for (t <- transitionsInto.getOrElse(l.getState, List())) {
                l.onInTransitionAdded(t, transitionToWeights(t), this)
            }
            for (nested <- nestedAutomatons) {
                nested.registerListener(l)
            }
        }
    }

    def addFinalState(state: D): Unit = {
        this.finalState += state
    }

    def registerDFSListener(state: D, l: ReachabilityListener[N, D]): Unit = {
        stateToReachabilityListener += (state -> l)
        if (dfsVisitor == null) {
            dfsVisitor = new ForwardDFSVisitor[N, D, W](this)
            this.registerListener(dfsVisitor)
        }
        dfsVisitor.registerListener(state, l)
    }

    protected def getStateToDFS: HashMap[D, ForwardDFSVisitor[N, D, W]] = {
        stateToDFS
    }

    def registerDFSEpsilonListener(state: D, l: ReachabilityListener[N, D]): Unit = {
        stateToEpsilonReachabilityListener += (state -> l)
        if (dfsEpsVisitor == null) {
            dfsEpsVisitor = new ForwardDFSEpsilonVisitor[N, D, W](this)
            this.registerListener(dfsEpsVisitor)
        }
        for (nested <- nestedAutomatons) {
            nested.registerDFSEpsilonListener(state, l)
        }
        dfsEpsVisitor.registerListener(state, l)
    }

    protected def getStateToEpsilonDFS: HashMap[D, ForwardDFSVisitor[N, D, W]] = {
        stateToEpsilonDFS
    }

    def getOne: W


    def createNestedAutomaton(initialState: D): WeightedPAutomaton[N, D, W] = {
        val nested = new WeightedPAutomaton[N, D, W] {

            override def createState(d: D, loc: N): D = createState(d, loc)

            override def epsilon(): N = epsilon()

            override def getOne(): W = getOne()

            override def isGeneratedState(d: D): Boolean = isGeneratedState(d)

            override protected def getStateToDFS(): HashMap[D, ForwardDFSVisitor[N, D, W]] = stateToDFS

            override protected def getStateToEpsilonDFS(): HashMap[D, ForwardDFSVisitor[N, D, W]] = stateToEpsilonDFS

            override def nested(): Boolean = true

            override def toString: String = "NESTED: \n" + super.toString
        }
        addNestedAutomaton(nested)
        nested
    }

    def registerUnbalancedPopListener(l: UnbalancedPopListener[N, D, W]): Unit = {
        if (unbalancedPopListeners.add(l)) {
            for ((t, value) <- unbalancedPops) {
                l.unbalancedPop(t.targetState, t.trans, value)
            }
        }
    }

    def unbalancedPop(targetState: D, trans: Transition[N, D], weight: W): Unit = {
        val t = new UnbalancedPopEntry(targetState, trans)
        val oldVal = unbalancedPops.get(t)
        val newVal = if (oldVal == null) weight else oldVal.combineWith(weight)
        if (!newVal.equals(oldVal)) {
            unbalancedPops.put(t, newVal)
            for (l <- unbalancedPopListeners.toList) {
                l.unbalancedPop(targetState, trans, newVal)
            }
        }
    }

    

    private var summaryEdges: HashSet[Transition[N, D]] = HashSet()
    private var summaryEdgeListener: HashSet[SummaryListener[N, D]] = HashSet()

    def registerSummaryEdge(t: Transition[N, D]): Unit = {
        if (summaryEdges.add(t)) {
            for (l <- summaryEdgeListener.toList) {
                l.addedSummary(t)
            }
        }
    }

    def addSummaryListener(l: SummaryListener[N, D]): Unit = {
        if (summaryEdgeListener.add(l)) {
            for (edge <- summaryEdges.toList) {
                l.addedSummary(edge)
            }
            for (nested <- nestedAutomatons.toList) {
                // Create a new SummaryListener of the correct type
                val correctTypeListener = new nested.SummaryListener[N, D] {
                    // Implement the methods of SummaryListener here, using l as needed
                }
                nested.addSummaryListener(correctTypeListener)
            }
        }
    }

    trait SummaryListener[N <: Location, D <: State] {
        def addedSummary(t: Transition[N, D]): Unit
    }

    class UnbalancedPopEntry(val targetState: D, trans: Transition[N, D]) {

        override def hashCode(): Int = {
            val prime = 31
            var result = 1
            result = prime * result + hashCode()
            result = prime * result + (if (targetState == null) 0 else targetState.hashCode())
            result = prime * result + (if (trans == null) 0 else trans.hashCode())
            result
        }

        override def equals(obj: Any): Boolean = obj match {
            case other: UnbalancedPopEntry =>
                (this eq other) || (other != null && getClass == other.getClass &&
                    hashCode() == other.hashCode() && targetState == other.targetState && trans == other.trans)
            case _ => false
        }
    }

    private class ReturnSiteWithWeights(val returnSite: N, val returnedFact: D, val returnedWeight: W) {

        override def hashCode(): Int = {
            val prime = 31
            var result = 1
            result = prime * result + getOuterType().hashCode()
            result = prime * result + (if (returnSite == null) 0 else returnSite.hashCode())
            result = prime * result + (if (returnedFact == null) 0 else returnedFact.hashCode())
            result
        }

        override def equals(obj: Any): Boolean = obj match {
            case other: ReturnSiteWithWeights =>
                (this eq other) || (other != null && getClass == other.getClass &&
                    getOuterType().equals(other.getOuterType()) && returnSite == other.returnSite &&
                    returnedFact == other.returnedFact && returnedWeight == other.returnedWeight)
            case _ => false
        }

        private def getOuterType(): WeightedPAutomaton[N, D, W] = WeightedPAutomaton.this
    }

    def getTransitionsToFinalWeights: HashMap[Transition[N, D], W] = {
        LOGGER.trace("Start computing final weights")
        val w = Stopwatch.createStarted()
        for (s <- initialStatesToSource.keySet) {
            registerListener(new ValueComputationListener(s, getOne))
        }
        LOGGER.trace("Finished computing final weights in {}", w)
        transitionsToFinalWeights
    }

    private class ValueComputationListener(state: D, var weight: W) extends WPAStateListener[N, D, W](state) {

        override def onOutTransitionAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {}

        override def onInTransitionAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
            val newWeight = weight.extendWith(w).asInstanceOf[W]
            val weightAtTarget = transitionsToFinalWeights.get(t)
            val newVal = weightAtTarget match {
                case Some(weight) => weight.combineWith(newWeight).asInstanceOf[W]
                case None => newWeight
            }
            transitionsToFinalWeights.put(t, newVal)
            if (isGeneratedState(t.getStart)) {
                registerListener(new ValueComputationListener(t.getStart, newVal))
            }
        }

        override def hashCode(): Int = {
            val prime = 31
            var result = super.hashCode()
            result = prime * result + getOuterType().hashCode()
            result = prime * result + (if (weight == null) 0 else weight.hashCode())
            result
        }

        override def equals(obj: Any): Boolean = obj match {
            case other: ValueComputationListener =>
                (this eq other) || (other != null && getClass == other.getClass &&
                    getOuterType().equals(other.getOuterType()) && weight == other.weight)
            case _ => false
        }

        private def getOuterType(): WeightedPAutomaton[N, D, W] = WeightedPAutomaton.this
    }

    def nested(): Boolean = false


    def addNestedAutomaton(nested: WeightedPAutomaton[N, D, W]): Unit = {
        if (!nestedAutomatons.add(nested)) return
        for (e <- stateListeners.values.toList) {
            nested.registerListener(e)
        }
        for (e <- listeners.toList) {
            nested.registerListener(e)
        }
        for (e <- summaryEdgeListener.toList) {
            // Create a new SummaryListener of the correct type
            val correctTypeListener = new nested.SummaryListener[N, D] {
                // Implement the methods of SummaryListener here, using e as needed
            }
            nested.addSummaryListener(correctTypeListener)
        }
        for (e <- unbalancedPopListeners.toList) {
            nested.registerUnbalancedPopListener(e)
        }
        for ((key, value) <- stateToEpsilonReachabilityListener) {
            nested.registerDFSEpsilonListener(key, value)
        }
        for ((key, value) <- stateToReachabilityListener) {
            nested.registerDFSListener(key, value)
        }
        for (e <- nestedAutomataListeners.toList) {
            e.nestedAutomaton(this, nested)
            nested.registerNestedAutomatonListener(e)
        }
    }

    def registerNestedAutomatonListener(l: NestedAutomatonListener[N, D, W]): Unit = {
        nestedAutomataListeners += l
        for (nested <- nestedAutomatons.toList) {
            l.nestedAutomaton(this, nested)
        }
    }

    def setInitialAutomaton(aut: WeightedPAutomaton[N, D, W]): Unit = {
        initialAutomaton = aut
    }

    def isInitialAutomaton(aut: WeightedPAutomaton[N, D, W]): Boolean = {
        initialAutomaton.equals(aut)
    }

    def toRegEx(start: D, end: D): IRegEx[N] = {
        if (lastStates < states.size) {
            pathExpressionComputer = new PathExpressionComputer[D, N](this)
            lastStates = states.size
        }
        RegEx.reverse(pathExpressionComputer.getExpressionBetween(end, start))
    }

    def containsLoop(): Boolean = {
        var visited = HashSet[D]()
        var worklist = initialStatesToSource.keySet.toList
        while (worklist.nonEmpty) {
            val pop = worklist.head
            worklist = worklist.tail
            visited += pop
            val inTrans = transitionsInto.getOrElse(pop, Set())
            for (t <- inTrans) {
                if (t.getLabel == this.epsilon) {}
                else if (!isGeneratedState(t.getStart)) {}
                else if (visited.contains(t.getStart)) {
                    return true
                }
                else {
                    worklist = t.getStart :: worklist
                }
            }
        }
        false
    }

    def getLongestPath(): HashSet[N] = {
        var worklist = initialStatesToSource.keySet.toList
        var pathReachingD = HashMap[D, HashSet[N]]()
        while (worklist.nonEmpty) {
            val pop = worklist.head
            worklist = worklist.tail
            val atCurr = pathReachingD.getOrElse(pop, Set())
            val inTrans = transitionsInto.getOrElse(pop, Set())
            for (t <- inTrans) {
                if (t.getLabel == this.epsilon) {}
                else if (!isGeneratedState(t.getStart)) {}
                else if (t.getStart == pop) {}
                else {
                    val next = t.getStart
                    val atNext = pathReachingD.getOrElse(next, Set())
                    val newAtCurr = atCurr + t.getLabel
                    if ((atNext ++ newAtCurr) != atNext) {
                        worklist = next :: worklist
                    }
                }
            }
        }
        var longest = HashSet[N]()
        for (l <- pathReachingD.values) {
            if (longest.size < l.size) {
                longest = l
            }
        }
        longest
    }

    private def getOrCreate(pathReachingD: mutable.HashMap[D, mutable.Set[N]], pop: D): mutable.Set[N] = {
        pathReachingD.getOrElseUpdate(pop, mutable.Set[N]())
    }

    def isUnbalancedState(target: D): Boolean = {
        initialStatesToSource.contains(target)
    }

    
    def addUnbalancedState(state: D, parent: D): Boolean = {
        var distance = 0
        val parents = HashSet[D]()
        if (!initialStatesToSource.contains(parent)) {
            distance = stateToUnbalancedDistance(parent)
            parents += parent
        } else {
            parents ++= initialStatesToSource(parent)
        }
        val newDistance = distance + 1
        stateToUnbalancedDistance.put(state, newDistance)
        if (getMaxUnbalancedDepth > 0 && newDistance > getMaxUnbalancedDepth) {
            return false
        }
        initialStatesToSource.put(state, parents)
        true
    }

    


    def addInitialState(state: D): Option[HashSet[D]] = {
        initialStatesToSource.put(state, HashSet(state))
    }

    def unregisterAllListeners(): Unit = {
        connectedPushListeners.clear()
        nestedAutomataListeners.clear()
        stateListeners.clear()
        listeners.clear()
        stateToEpsilonReachabilityListener.clear()
        stateToReachabilityListener.clear()
        summaryEdgeListener.clear()
        unbalancedPopListeners.clear()
    }

    def getWatch: Stopwatch = watch

    def hasMaxDepth: Boolean = getMaxDepth > 0

    def getMaxDepth: Int = -1

    def getMaxUnbalancedDepth: Int = -1

    def getUnbalancedStartOf(target: D): HashSet[D] = initialStatesToSource(target)

}