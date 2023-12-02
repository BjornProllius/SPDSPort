package boomerang.poi

import boomerang.{BackwardQuery, ForwardBoomerangSolver, BackwardBoomerangSolver}
import boomerang.scene.{ControlFlowGraph, Field, Statement, Val}
import com.google.common.base.Objects
import com.google.common.collect.{HashMultimap, Lists, Multimap, Sets}
import sync.pds.solver.nodes.{INode, Node, SingleNode}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.WPAStateListener

class CopyAccessPathChain[W <: Weight]{
  private val MAX_WALK_DEPTH = -1
  private var forwardSolver: ForwardBoomerangSolver[W] = _
  private var backwardSolver: BackwardBoomerangSolver[W] = _
  private var fieldWriteStatement: Edge = _
  // TODO: Source of non-determinism: not part of hashCode/equals....but also shall not be.
  private var killedTransitionTarget: INode[Node[Edge, Val]] = _

  def this(forwardSolver: ForwardBoomerangSolver[W], backwardSolver: BackwardBoomerangSolver[W], fieldWriteStatement: Edge, killedTransition: Transition[Field, INode[Node[Edge, Val]]]) {
    this()
    this.forwardSolver = forwardSolver
    this.backwardSolver = backwardSolver
    this.fieldWriteStatement = fieldWriteStatement
    this.killedTransitionTarget = killedTransition.getTarget
  }

  def exec(): Unit = {
    forwardSolver
      .getFieldAutomaton
      .registerListener(
        new WalkForwardSolverListener(
          killedTransitionTarget,
          new SingleNode[Node[Edge, Val]](
            new Node[Edge, Val](fieldWriteStatement, fieldWriteStatement.getTarget.getRightOp)),
          0))
  }

  private class WalkForwardSolverListener
    extends WPAStateListener[Field, INode[Node[Edge, Val]], W]{
private var stateInBwSolver: INode[Node[Edge, Val]] = _
private var walkDepth: Int = _

def this(target: INode[Node[Edge, Val]], stateInBwSolver: INode[Node[Edge, Val]], walkDepth: Int) {
  this()
  this.stateInBwSolver = stateInBwSolver
  this.walkDepth = walkDepth
}

override def onOutTransitionAdded(
    t: Transition[Field, INode[Node[Edge, Val]]],
    w: W,
    weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {
  if (t.getLabel == Field.empty()) {
    if (forwardSolver.getFieldAutomaton.isUnbalancedState(t.getTarget)) {
      if (t.getStart == CopyAccessPathChain.this.killedTransitionTarget) {
        // Do a simple backwardSolve(...)...
        val query = BackwardQuery.make(
          fieldWriteStatement, fieldWriteStatement.getTarget.getRightOp)
        val fieldTarget = backwardSolver.createQueryNodeField(query)
        val callTarget = backwardSolver.generateCallState(
          new SingleNode(query.`var`()), query.cfgEdge())
        backwardSolver.solve(
          query.asNode(), Field.empty(), fieldTarget, query.cfgEdge(), callTarget)
        return
      }
      // addReachable(stateInBwSolver);
    }
    return
  }
  val targetState = backwardSolver.generateFieldState(
    new SingleNode(
      new Node(new Edge(Statement.epsilon(), Statement.epsilon()), Val.zero())),
    t.getLabel)
  val insert = new Transition(stateInBwSolver, t.getLabel, targetState)
  queueOrAdd(insert)
  val newDepth = walkDepth + 1
  if (MAX_WALK_DEPTH < 0 || newDepth < MAX_WALK_DEPTH) {
    forwardSolver
      .getFieldAutomaton
      .registerListener(new WalkForwardSolverListener(t.getTarget, targetState, newDepth))
  }
}

    override def onInTransitionAdded(
        t: Transition[Field, INode[Node[Edge, Val]]],
        w: W,
        weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {}

    override def equals(o: Any): Boolean = o match {
      case that: WalkForwardSolverListener =>
        (that canEqual this) &&
          getEnclosingInstance == that.getEnclosingInstance &&
          stateInBwSolver == that.stateInBwSolver
      case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[WalkForwardSolverListener]

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (stateInBwSolver == null) 0 else stateInBwSolver.hashCode)
      result
    }

    private def getEnclosingInstance: CopyAccessPathChain = CopyAccessPathChain.this
  }

  // Copied from ExecuteImportFielStmtPOI

  private val reachable: Set[INode[Node[Edge, Val]]] = new HashSet[INode[Node[Edge, Val]]]()
  private val delayedTransitions: MultiMap[INode[Node[Edge, Val]], InsertFieldTransitionCallback] = new HashMap[INode[Node[Edge, Val]], Set[InsertFieldTransitionCallback]] with MultiMap[INode[Node[Edge, Val]], InsertFieldTransitionCallback]

  def addReachable(node: INode[Node[Edge, Val]]): Unit = {
    if (reachable.add(node)) {
      delayedTransitions.get(node).foreach(callback => callback.trigger())
    }
  }

  private def queueOrAdd(transToInsert: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
    if (reachable.contains(transToInsert.getTarget)) {
      backwardSolver.getFieldAutomaton.addTransition(transToInsert)
    } else {
      delayedTransitions.addBinding(transToInsert.getTarget, new InsertFieldTransitionCallback(transToInsert))
    }
  }

  private class InsertFieldTransitionCallback(trans: Transition[Field, INode[Node[Edge, Val]]]) {
    def trigger(): Unit = {
      backwardSolver.getFieldAutomaton.addTransition(trans)
      addReachable(trans.getStart)
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (trans == null) 0 else trans.hashCode)
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case that: InsertFieldTransitionCallback =>
        (that canEqual this) &&
          getEnclosingInstance == that.getEnclosingInstance &&
          trans == that.trans
      case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[InsertFieldTransitionCallback]

    private def getEnclosingInstance: CopyAccessPathChain = CopyAccessPathChain.this
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (backwardSolver == null) 0 else backwardSolver.hashCode())
    result = prime * result + (if (fieldWriteStatement == null) 0 else fieldWriteStatement.hashCode())
    result = prime * result + (if (forwardSolver == null) 0 else forwardSolver.hashCode())
    result = prime * result + (if (killedTransitionTarget == null) 0 else killedTransitionTarget.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: CopyAccessPathChain =>
      (that canEqual this) &&
        backwardSolver == that.backwardSolver &&
        fieldWriteStatement == that.fieldWriteStatement &&
        forwardSolver == that.forwardSolver &&
        killedTransitionTarget == that.killedTransitionTarget
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[CopyAccessPathChain]
}
