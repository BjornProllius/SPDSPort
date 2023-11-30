package sync.pds.solver

import com.google.common.base.Objects
import com.google.common.collect._
import java.util.{AbstractMap, Collection, HashSet, Map, Set}
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes._
import wpds.impl._
import wpds.interfaces._


import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.WeightedPushdownSystem
import wpds.interfaces.Location
import wpds.interfaces.Weight

class SyncPDSSolver[Stmt <: Location, Fact, Field <: Location, W <: Weight] {

    object PDSSystem extends Enumeration {
        type PDSSystem = Value
        val FIELDS, CALLS = Value
    }

    private val logger = LoggerFactory.getLogger(classOf[SyncPDSSolver[Stmt, Fact, Field, W]])
    private val FieldSensitive = true
    private val ContextSensitive = true

    protected val callingPDS: WeightedPushdownSystem[Stmt, INode[Fact], W] = new WeightedPushdownSystem[Stmt, INode[Fact], W] {
        override def toString: String = "Call " + SyncPDSSolver.this.toString
    }

    protected val fieldPDS: WeightedPushdownSystem[Field, INode[Node[Stmt, Fact]], W] = new WeightedPushdownSystem[Field, INode[Node[Stmt, Fact]], W] {
        override def toString: String = "Field " + SyncPDSSolver.this.toString
    }


import com.google.common.collect.{HashMultimap, Multimap, Sets}
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.{Transition, WeightedPAutomaton}
import wpds.interfaces.Location
import wpds.interfaces.Weight

import scala.collection.JavaConverters._

class SyncPDSSolver[Stmt <: Location, Fact, Field <: Location, W <: Weight] {

  private val reachedStates: Set[Node[Stmt, Fact]] = Sets.newHashSet().asScala.toSet
  private val callingContextReachable: Set[Node[Stmt, Fact]] = Sets.newHashSet().asScala.toSet
  private val fieldContextReachable: Set[Node[Stmt, Fact]] = Sets.newHashSet().asScala.toSet
  private val updateListeners: Set[SyncPDSUpdateListener[Stmt, Fact]] = Sets.newHashSet().asScala.toSet
  private val reachedStateUpdateListeners: Multimap[Node[Stmt, Fact], SyncStatePDSUpdateListener[Stmt, Fact]] = HashMultimap.create()
  protected val fieldAutomaton: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W] = _
  protected val callAutomaton: WeightedPAutomaton[Stmt, INode[Fact], W] = _

  protected def preventFieldTransitionAdd(trans: Transition[Field, INode[Node[Stmt, Fact]]], weight: W): Boolean = false

  protected def preventCallTransitionAdd(trans: Transition[Stmt, INode[Fact]], weight: W): Boolean = false
}

class SyncPDSSolver[Stmt <: Location, Fact, Field <: Location, W <: Weight](
    useCallSummaries: Boolean,
    callSummaries: NestedWeightedPAutomatons[Stmt, INode[Fact], W],
    useFieldSummaries: Boolean,
    fieldSummaries: NestedWeightedPAutomatons[Field, INode[Node[Stmt, Fact]], W],
    maxCallDepth: Int,
    maxFieldDepth: Int,
    maxUnbalancedCallDepth: Int) {

    fieldAutomaton = new WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W] {
        override def createState(d: INode[Node[Stmt, Fact]], loc: Field): INode[Node[Stmt, Fact]] = {
            if (loc == emptyField()) d else generateFieldState(d, loc)
        }

        override def epsilon(): Field = epsilonField()

        override def nested(): Boolean = useFieldSummaries

        override def getOne(): W = getFieldWeights().getOne()

        override def getMaxDepth(): Int = maxFieldDepth

        override def addWeightForTransition(trans: Transition[Field, INode[Node[Stmt, Fact]]], weight: W): Boolean = {
            if (preventFieldTransitionAdd(trans, weight)) false
            else {
                logger.trace("Adding field transition {} with weight {}", trans, weight)
                super.addWeightForTransition(trans, weight)
            }
        }

        override def isGeneratedState(d: INode[Node[Stmt, Fact]]): Boolean = d.isInstanceOf[GeneratedState]
    }

    callAutomaton = new WeightedPAutomaton[Stmt, INode[Fact], W] {
        override def createState(d: INode[Fact], loc: Stmt): INode[Fact] = generateCallState(d, loc)

        override def epsilon(): Stmt = epsilonStmt()

        override def nested(): Boolean = useCallSummaries

        override def getOne(): W = getCallWeights().getOne()

        override def addWeightForTransition(trans: Transition[Stmt, INode[Fact]], weight: W): Boolean = {
            if (preventCallTransitionAdd(trans, weight)) false
            else {
                logger.trace("Adding call transition {} with weight {}", trans, weight)
                super.addWeightForTransition(trans, weight)
            }
        }

        override def isGeneratedState(d: INode[Fact]): Boolean = d.isInstanceOf[GeneratedState]

        override def getMaxDepth(): Int = maxCallDepth

        override def getMaxUnbalancedDepth(): Int = maxUnbalancedCallDepth
    }

    callAutomaton.registerListener(new CallAutomatonListener())
    fieldAutomaton.registerListener(new FieldUpdateListener())
    if (callAutomaton.nested())
        callAutomaton.registerNestedAutomatonListener(new CallSummaryListener())
    // if(fieldAutomaton.nested())
    // fieldAutomaton.registerNestedAutomatonListener(new FieldSummaryListener())   
    callingPDS.poststar(callAutomaton, callSummaries)
    fieldPDS.poststar(fieldAutomaton, fieldSummaries)
}

private class FieldSummaryListener extends NestedAutomatonListener[Field, INode[Node[Stmt, Fact]], W] {
    override def nestedAutomaton(parent: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W], child: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]): Unit = {
        for (s <- child.getInitialStates) {
            child.registerListener(new FieldAddEpsilonToInitialStateListener(s, parent))
        }
    }
}

private class FieldAddEpsilonToInitialStateListener(state: INode[Node[Stmt, Fact]], parent: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]) extends WPAStateListener[Field, INode[Node[Stmt, Fact]], W](state) {

    override def onOutTransitionAdded(t: Transition[Field, INode[Node[Stmt, Fact]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]): Unit = {}

    override def onInTransitionAdded(nestedT: Transition[Field, INode[Node[Stmt, Fact]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]): Unit = {
        if (nestedT.getLabel == fieldAutomaton.epsilon()) {
            parent.registerListener(new FieldOnOutTransitionAddToStateListener(this.getState, nestedT))
        }
    }

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + SyncPDSSolver.this.hashCode()
        result = prime * result + (if (parent == null) 0 else parent.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: FieldAddEpsilonToInitialStateListener =>
                (this eq other) || (super.equals(obj) && (this.getClass == obj.getClass) && SyncPDSSolver.this.equals(other.SyncPDSSolver.this) && (if (parent == null) other.parent == null else parent.equals(other.parent)))
            case _ => false
        }
    }
}

private class FieldOnOutTransitionAddToStateListener(state: INode[Node[Stmt, Fact]], var nestedT: Transition[Field, INode[Node[Stmt, Fact]]]) extends WPAStateListener[Field, INode[Node[Stmt, Fact]], W](state) {

    override def onOutTransitionAdded(t: Transition[Field, INode[Node[Stmt, Fact]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]): Unit = {
        setFieldContextReachable(nestedT.getStart.fact())
    }

    override def onInTransitionAdded(t: Transition[Field, INode[Node[Stmt, Fact]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]): Unit = {}

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + SyncPDSSolver.this.hashCode()
        result = prime * result + (if (nestedT == null) 0 else nestedT.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: FieldOnOutTransitionAddToStateListener =>
                (this eq other) || (super.equals(obj) && (this.getClass == obj.getClass) && SyncPDSSolver.this.equals(other.SyncPDSSolver.this) && (if (nestedT == null) other.nestedT == null else nestedT.equals(other.nestedT)))
            case _ => false
        }
    }
}

private class CallSummaryListener extends NestedAutomatonListener[Stmt, INode[Fact], W] {
    override def nestedAutomaton(parent: WeightedPAutomaton[Stmt, INode[Fact], W], child: WeightedPAutomaton[Stmt, INode[Fact], W]): Unit = {
        for (s <- child.getInitialStates) {
            child.registerListener(new AddEpsilonToInitialStateListener(s, parent))
        }
    }
}

private class AddEpsilonToInitialStateListener(state: INode[Fact], var parent: WeightedPAutomaton[Stmt, INode[Fact], W]) extends WPAStateListener[Stmt, INode[Fact], W](state) {

    override def onOutTransitionAdded(t: Transition[Stmt, INode[Fact]], w: W, weightedPAutomaton: WeightedPAutomaton[Stmt, INode[Fact], W]): Unit = {}

    override def onInTransitionAdded(nestedT: Transition[Stmt, INode[Fact]], w: W, weightedPAutomaton: WeightedPAutomaton[Stmt, INode[Fact], W]): Unit = {
        if (nestedT.getLabel == callAutomaton.epsilon()) {
            parent.registerListener(new OnOutTransitionAddToStateListener(this.getState, nestedT))
        }
    }

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + SyncPDSSolver.this.hashCode()
        result = prime * result + (if (parent == null) 0 else parent.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: AddEpsilonToInitialStateListener =>
                (this eq other) || (super.equals(obj) && (this.getClass == obj.getClass) && SyncPDSSolver.this.equals(other.SyncPDSSolver.this) && (if (parent == null) other.parent == null else parent.equals(other.parent)))
            case _ => false
        }
    }
}

private class OnOutTransitionAddToStateListener(state: INode[Fact], var nestedT: Transition[Stmt, INode[Fact]]) extends WPAStateListener[Stmt, INode[Fact], W](state) {

    override def onOutTransitionAdded(t: Transition[Stmt, INode[Fact]], w: W, weightedPAutomaton: WeightedPAutomaton[Stmt, INode[Fact], W]): Unit = {
        val returningNode = new Node[Stmt, Fact](t.getLabel, nestedT.getStart.fact)
        setCallingContextReachable(returningNode)
    }

    override def onInTransitionAdded(t: Transition[Stmt, INode[Fact]], w: W, weightedPAutomaton: WeightedPAutomaton[Stmt, INode[Fact], W]): Unit = {}

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + SyncPDSSolver.this.hashCode()
        result = prime * result + (if (nestedT == null) 0 else nestedT.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: OnOutTransitionAddToStateListener =>
                (this eq other) || (super.equals(obj) && (this.getClass == obj.getClass) && SyncPDSSolver.this.equals(other.SyncPDSSolver.this) && (if (nestedT == null) other.nestedT == null else nestedT.equals(other.nestedT)))
            case _ => false
        }
    }
}

private class CallAutomatonListener extends WPAUpdateListener[Stmt, INode[Fact], W] {

    override def onWeightAdded(t: Transition[Stmt, INode[Fact]], w: W, aut: WeightedPAutomaton[Stmt, INode[Fact], W]): Unit = {
        if (!t.getStart.isInstanceOf[GeneratedState] && !t.getLabel.equals(callAutomaton.epsilon())) {
            val node = new Node[Stmt, Fact](t.getLabel, t.getStart.fact())
            setCallingContextReachable(node)
        }
    }
}

def solve(curr: Node[Stmt, Fact], field: Field, fieldTarget: INode[Node[Stmt, Fact]], stmt: Stmt, callTarget: INode[Fact], weight: W): Unit = {
    fieldAutomaton.addInitialState(fieldTarget)
    callAutomaton.addInitialState(callTarget)
    val start = asFieldFact(curr)
    if (!field.equals(emptyField())) {
        val generateFieldState = generateFieldState(start, field)
        val fieldTrans = new Transition[Field, INode[Node[Stmt, Fact]]](start, field, generateFieldState)
        fieldAutomaton.addTransition(fieldTrans)
        val fieldTransToInitial = new Transition[Field, INode[Node[Stmt, Fact]]](generateFieldState, emptyField(), fieldTarget)
        fieldAutomaton.addTransition(fieldTransToInitial)
    } else {
        val fieldTrans = new Transition[Field, INode[Node[Stmt, Fact]]](start, emptyField(), fieldTarget)
        fieldAutomaton.addTransition(fieldTrans)
    }
    val callTrans = new Transition[Stmt, INode[Fact]](wrap(curr.fact()), curr.stmt(), callTarget)
    callAutomaton.addWeightForTransition(callTrans, weight)
    processNode(curr)
}

def solve(curr: Node[Stmt, Fact], field: Field, fieldTarget: INode[Node[Stmt, Fact]], stmt: Stmt, callTarget: INode[Fact]): Unit = {
    solve(curr, field, fieldTarget, stmt, callTarget, getCallWeights().getOne())
}

def processNode(curr: Node[Stmt, Fact]): Unit = {
    if (!addReachableState(curr)) return
    computeSuccessor(curr)
}

def propagate(curr: Node[Stmt, Fact], s: State): Unit = {
    s match {
        case succ: Node[Stmt, Fact] =>
            succ match {
                case pushNode: PushNode[Stmt, Fact, Location] =>
                    val system = pushNode.system()
                    val location = pushNode.location()
                    processPush(curr, location, pushNode, system)
                case _ => processNormal(curr, succ)
            }
        case popNode: PopNode[Fact] => processPop(curr, popNode)
        case _ =>
    }
}

private def addReachableState(curr: Node[Stmt, Fact]): Boolean = {
    if (reachedStates.contains(curr)) return false
    reachedStates.add(curr)
    for (l <- Lists.newLinkedList(updateListeners)) {
        l.onReachableNodeAdded(curr)
    }
    for (l <- Lists.newLinkedList(reachedStateUpdateListeners.get(curr))) {
        l.reachable()
    }
    true
}

def processNormal(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact]): Unit = {
    addNormalFieldFlow(curr, succ)
    addNormalCallFlow(curr, succ)
}

def addNormalCallFlow(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact]): Unit = {
    addCallRule(
        new NormalRule(
            wrap(curr.fact),
            curr.stmt,
            wrap(succ.fact),
            succ.stmt,
            getCallWeights.normal(curr, succ)))
}

def addNormalFieldFlow(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact]): Unit = {
    succ match {
        case exNode: ExclusionNode[Stmt, Fact, Field] =>
            addFieldRule(
                new NormalRule(
                    asFieldFact(curr),
                    fieldWildCard(),
                    asFieldFact(succ),
                    exclusionFieldWildCard(exNode.exclusion),
                    getFieldWeights.normal(curr, succ)))
            return
        case _ =>
            addFieldRule(
                new NormalRule(
                    asFieldFact(curr),
                    fieldWildCard(),
                    asFieldFact(succ),
                    fieldWildCard(),
                    getFieldWeights.normal(curr, succ)))
    }
}

def asFieldFact(node: Node[Stmt, Fact]): INode[Node[Stmt, Fact]] = {
    new SingleNode(new Node(node.stmt, node.fact))
}

def processPop(curr: Node[Stmt, Fact], popNode: PopNode): Unit = {
    val system = popNode.system
    val location = popNode.location
    system match {
        case PDSSystem.FIELDS =>
            val node = location.asInstanceOf[NodeWithLocation[Stmt, Fact, Field]]
            if (FieldSensitive) {
                addFieldRule(
                    new PopRule(
                        asFieldFact(curr),
                        node.location,
                        asFieldFact(node.fact),
                        getFieldWeights.pop(curr)))
            } else {
                addNormalFieldFlow(curr, node.fact)
            }
            addNormalCallFlow(curr, node.fact)
        case PDSSystem.CALLS =>
            if (ContextSensitive) {
                addCallRule(
                    new PopRule(
                        wrap(curr.fact), curr.stmt, wrap(location.asInstanceOf[Fact]), getCallWeights.pop(curr)))
            }
        case _ =>
    }
}

private def applyCallSummary(callSite: Stmt, factInCallee: Fact, spInCallee: Stmt): Unit = {
    callAutomaton.addSummaryListener(
        t => {
            val genSt = t.getTarget().asInstanceOf[GeneratedState[Fact, Stmt]]
            val sp = genSt.location()
            val v = genSt.node().fact()
            val exitStmt = t.getLabel()
            val returnedFact = t.getStart().fact()
            if (spInCallee == sp && factInCallee == v) {
                if (summaries.add(
                    new Summary(callSite, factInCallee, spInCallee, exitStmt, returnedFact))) {
                    for (s <- summaryListeners.toList) {
                        s.apply(callSite, factInCallee, spInCallee, exitStmt, returnedFact)
                    }
                }
                applyCallSummary(callSite, factInCallee, spInCallee, exitStmt, returnedFact)
            }
        })
}

var summaries: Set[Summary] = Set()
var summaryListeners: Set[OnAddedSummaryListener] = Set()

def addApplySummaryListener(l: OnAddedSummaryListener): Unit = {
    if (summaryListeners.add(l)) {
        for (s <- summaries.toList) {
            l.apply(s.callSite, s.factInCallee, s.spInCallee, s.exitStmt, s.returnedFact)
        }
    }
}

trait OnAddedSummaryListener {
    def apply(callSite: Stmt, factInCallee: Fact, spInCallee: Stmt, exitStmt: Stmt, returnedFact: Fact): Unit
}

private class Summary(val callSite: Stmt, val factInCallee: Fact, val spInCallee: Stmt, val exitStmt: Stmt, val returnedFact: Fact) {

    override def equals(o: Any): Boolean = o match {
        case that: Summary =>
            callSite == that.callSite &&
                factInCallee == that.factInCallee &&
                spInCallee == that.spInCallee &&
                exitStmt == that.exitStmt &&
                returnedFact == that.returnedFact
        case _ => false
    }

    override def hashCode(): Int = {
        val state = Seq(callSite, factInCallee, spInCallee, exitStmt, returnedFact)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
}

def applyCallSummary(callSite: Stmt, factInCallee: Fact, spInCallee: Stmt, exitStmt: Stmt, returnedFact: Fact): Unit

def processPush(curr: Node[Stmt, Fact], location: Location, succ: PushNode[Stmt, Fact, _], system: PDSSystem): Unit = {
    if (system == PDSSystem.FIELDS) {
        if (FieldSensitive) {
            addFieldRule(
                new PushRule(
                    asFieldFact(curr),
                    fieldWildCard(),
                    asFieldFact(succ),
                    location.asInstanceOf[Field],
                    fieldWildCard(),
                    getFieldWeights.push(curr, succ, location.asInstanceOf[Field])))
        } else {
            addNormalFieldFlow(curr, succ)
        }
        addNormalCallFlow(curr, succ)
    } else if (system == PDSSystem.CALLS) {
        addNormalFieldFlow(curr, succ)
        if (ContextSensitive) {
            addCallRule(
                new PushRule(
                    wrap(curr.fact),
                    curr.stmt,
                    wrap(succ.fact),
                    succ.stmt,
                    location.asInstanceOf[Stmt],
                    getCallWeights.push(curr, succ, location.asInstanceOf[Stmt])))
        } else {
            addNormalCallFlow(curr, succ)
        }
        applyCallSummary(location.asInstanceOf[Stmt], succ.fact, succ.stmt)
    }
}

def addCallRule(rule: Rule[Stmt, INode[Fact], W]): Unit = {
    callingPDS.addRule(rule)
}

def addFieldRule(rule: Rule[Field, INode[Node[Stmt, Fact]], W]): Unit = {
    fieldPDS.addRule(rule)
}

def getFieldWeights(): WeightFunctions[Stmt, Fact, Field, W]

def getCallWeights(): WeightFunctions[Stmt, Fact, Stmt, W]

private class FieldUpdateListener extends WPAUpdateListener[Field, INode[Node[Stmt, Fact]], W] {

    override def onWeightAdded(t: Transition[Field, INode[Node[Stmt, Fact]]], w: W, aut: WeightedPAutomaton[Field, INode[Node[Stmt, Fact]], W]): Unit = {
        val n = t.getStart
        if (!n.isInstanceOf[GeneratedState] && !t.getLabel.equals(fieldAutomaton.epsilon())) {
            val fact = n.fact()
            val node = new Node[Stmt, Fact](fact.stmt(), fact.fact())
            setFieldContextReachable(node)
        }
    }
}

private def setCallingContextReachable(node: Node[Stmt, Fact]): Unit = {
    if (!callingContextReachable.add(node)) return
    if (fieldContextReachable.contains(node)) {
        processNode(node)
    }
}

private def setFieldContextReachable(node: Node[Stmt, Fact]): Unit = {
    if (!fieldContextReachable.add(node)) return
    if (callingContextReachable.contains(node)) {
        processNode(node)
    }
}

def registerListener(listener: SyncPDSUpdateListener[Stmt, Fact]): Unit = {
    if (!updateListeners.add(listener)) {
        return
    }
    for (reachableNode <- reachedStates.toList) {
        listener.onReachableNodeAdded(reachableNode)
    }
}

def registerListener(listener: SyncStatePDSUpdateListener[Stmt, Fact]): Unit = {
    if (!reachedStateUpdateListeners.put(listener.getNode(), listener)) {
        return
    }
    if (reachedStates.contains(listener.getNode())) {
        listener.reachable()
    }
}

protected def wrap(variable: Fact): INode[Fact] = {
    new SingleNode[Fact](variable)
}

protected var generatedCallState: Map[(INode[Fact], Stmt), INode[Fact]] = Map()

def generateCallState(d: INode[Fact], loc: Stmt): INode[Fact] = {
    val e = (d, loc)
    if (!generatedCallState.contains(e)) {
        generatedCallState += (e -> new GeneratedState[Fact, Stmt](d, loc))
    }
    generatedCallState(e)
}

var generatedFieldState: Map[(INode[Node[Stmt, Fact]], Field), INode[Node[Stmt, Fact]]] = Map()

def generateFieldState(d: INode[Node[Stmt, Fact]], loc: Field): INode[Node[Stmt, Fact]] = {
    val e = (d, loc)
    if (!generatedFieldState.contains(e)) {
        generatedFieldState += (e -> new GeneratedState[Node[Stmt, Fact], Field](d, loc))
    }
    generatedFieldState(e)
}

def addGeneratedFieldState(state: GeneratedState[Node[Stmt, Fact], Field]): Unit = {
    val e = (state.node(), state.location())
    generatedFieldState += (e -> state)
}

def computeSuccessor(node: Node[Stmt, Fact]): Unit

def epsilonField(): Field

def emptyField(): Field

def epsilonStmt(): Stmt

def exclusionFieldWildCard(exclusion: Field): Field

def fieldWildCard(): Field

def getReachedStates(): Set[Node[Stmt, Fact]] = {
    reachedStates.toSet
}
def debugOutput(): Unit = {
    logger.debug(this.getClass.toString)
    logger.debug("All reachable states")
    prettyPrintSet(getReachedStates)

    val notFieldReachable = callingContextReachable.toSet -- getReachedStates
    val notCallingContextReachable = fieldContextReachable.toSet -- getReachedStates
    if (notFieldReachable.nonEmpty) {
        logger.debug("Calling context reachable")
        prettyPrintSet(notFieldReachable)
    }
    if (notCallingContextReachable.nonEmpty) {
        logger.debug("Field matching reachable")
        prettyPrintSet(notCallingContextReachable)
    }
    logger.debug(fieldPDS.toString)
    logger.debug(fieldAutomaton.toDotString)
    logger.debug(callingPDS.toString)
    logger.debug(callAutomaton.toDotString)
    logger.debug("===== end === " + this.getClass)
}

private def prettyPrintSet(set: Set[_]): Unit = {
    var j = 0
    var s = ""
    for (reachableState <- set) {
        s += reachableState
        s += "\t"
        if (j > 5) {
            s += "\n"
            j = 0
        }
        j += 1
    }
    logger.debug(s)
}
}