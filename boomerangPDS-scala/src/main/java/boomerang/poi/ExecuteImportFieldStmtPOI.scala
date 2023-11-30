package boomerang.poi

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ControlFlowEdgeBasedCallTransitionListener
import boomerang.solver.ControlFlowEdgeBasedFieldTransitionListener
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.collect.{HashMultimap, Lists, Multimap, Sets}
import java.util.Set
import org.slf4j.{Logger, LoggerFactory}
import sync.pds.solver.nodes.{GeneratedState, INode, Node, SingleNode}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.{WPAStateListener, WPAUpdateListener}

abstract class ExecuteImportFieldStmtPOI[W <: Weight] {
  
  abstract class ExecuteImportFieldStmtPOI[W <: Weight] {
    private val LOGGER = LoggerFactory.getLogger(classOf[ExecuteImportFieldStmtPOI[_]])
    private val MAX_IMPORT_DEPTH = -1
    private var reachable = Sets.newHashSet[INode[Node[Edge, Val]]]()
    private var delayedTransitions = HashMultimap.create[INode[Node[Edge, Val]], InsertFieldTransitionCallback]()
    protected val baseSolver: ForwardBoomerangSolver[W]
    protected val flowSolver: ForwardBoomerangSolver[W]
    protected val curr: Edge
    protected val baseAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
    protected val flowAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
    private val baseVar: Val
    private val storedVar: Val
    private val field: Field
    var active = false

    def this(baseSolver: ForwardBoomerangSolver[W], flowSolver: ForwardBoomerangSolver[W], poi: AbstractPOI[Edge, Val, Field]) {
      this()
      this.baseSolver = baseSolver
      this.flowSolver = flowSolver
      this.baseAutomaton = baseSolver.getFieldAutomaton()
      this.flowAutomaton = flowSolver.getFieldAutomaton()
      this.curr = poi.getCfgEdge()
      this.baseVar = poi.getBaseVar()
      this.storedVar = poi.getStoredVar()
      this.field = poi.getField()
    }

    private def isLogEnabled(): Boolean = {
      true
    }
  }

  private final class ImportTransitionFromCall(flowSolver: AbstractBoomerangSolver[W], stmt: Edge, start: INode[Val], target: INode[Val], w: W) extends ControlFlowEdgeBasedCallTransitionListener[W](stmt) {

    override def onAddedTransition(t: Transition[Edge, INode[Val]], w: W): Unit = {
      if (t.getStart().isInstanceOf[GeneratedState]) return
      val newTrans = new Transition[Edge, INode[Val]](t.getStart(), t.getLabel(), target)
      if (isLogEnabled()) {
        LOGGER.trace("Copying {} to {}", newTrans, flowSolver)
      }
      if (!t.getStart().equals(start)) {
        if (t.getStart().fact().m().equals(t.getLabel().getStart().getMethod())) {
          flowSolver.getCallAutomaton().addWeightForTransition(newTrans, this.w)
        }
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
      result = prime * result + (if (target == null) 0 else target.hashCode())
      result = prime * result + (if (w == null) 0 else w.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ImportTransitionFromCall =>
          (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)) &&
          (this.target == null && other.target == null || this.target != null && this.target.equals(other.target)) &&
          (this.w == null && other.w == null || this.w != null && this.w.equals(other.w))
        case _ => false
      }
    }
  }

  private final class ImportOnReachStatement(flowSolver: AbstractBoomerangSolver[W], callSiteOrExitStmt: Edge) extends ControlFlowEdgeBasedCallTransitionListener[W](callSiteOrExitStmt) {

    override def onAddedTransition(t: Transition[Edge, INode[Val]], w: W): Unit = {
      if (t.getStart().isInstanceOf[GeneratedState]) {
        return
      }
      if (t.getLabel().equals(getControlFlowEdge())) {
        baseSolver.registerStatementFieldTransitionListener(
          new CallSiteOrExitStmtFieldImport(
            flowSolver, baseSolver, new Node(t.getLabel(), t.getStart().fact())))
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ImportOnReachStatement =>
          (this == other) ||
          (super.equals(other) && (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)))
        case _ => false
      }
    }
  }

  private class ForAnyCallSiteOrExitStmt(baseSolver: AbstractBoomerangSolver[W]) extends WPAUpdateListener[Edge, INode[Val], W] {
    
    override def onWeightAdded(t: Transition[Edge, INode[Val]], w: W, aut: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
      if (!flowSolver.getCallAutomaton().isUnbalancedState(t.getTarget())) return
      if (t.getLabel().equals(new Edge(Statement.epsilon(), Statement.epsilon()))) {
        return
      }
      val edge = t.getLabel()
      val callSite = edge.getStart()
      if (callSite.containsInvokeExpr()) {
        if (callSite.isAssign() && callSite.getLeftOp().equals(t.getStart().fact())) return
        if (callSite.uses(t.getStart().fact())) {
          importSolvers(edge, t.getStart(), t.getTarget(), w)
        }
      }
    }

    private def importSolvers(callSiteOrExitStmt: Edge, start: INode[Val], node: INode[Val], w: W): Unit = {
      if (isLogEnabled()) {
        LOGGER.trace(
          "Importing solvers at {} from {} to {}", callSiteOrExitStmt, baseSolver, flowSolver)
      }
      baseSolver.registerStatementCallTransitionListener(
        new ImportOnReachStatement(flowSolver, callSiteOrExitStmt))
      baseSolver.registerStatementCallTransitionListener(
        new ImportTransitionFromCall(flowSolver, callSiteOrExitStmt, start, node, w))
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (baseSolver == null) 0 else baseSolver.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ForAnyCallSiteOrExitStmt =>
          (this == other) ||
          (this.baseSolver == null && other.baseSolver == null || this.baseSolver != null && this.baseSolver.equals(other.baseSolver))
        case _ => false
      }
    }
  }

  def solve(): Unit = {
    if (baseSolver == flowSolver) {
      return
    }
    baseSolver.registerStatementFieldTransitionListener(new BaseVarPointsTo(curr, this))
  }

  private class BaseVarPointsTo(curr: Edge, poi: ExecuteImportFieldStmtPOI[W]) extends ControlFlowEdgeBasedFieldTransitionListener[W](curr) {

    override def onAddedTransition(t: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
      val aliasedVariableAtStmt = t.getStart()
      if (active) return
      if (!aliasedVariableAtStmt.isInstanceOf[GeneratedState]) {
        val alias = aliasedVariableAtStmt.fact().fact()
        if (alias == poi.baseVar && t.getLabel() == Field.empty()) {
          flowsTo()
        }
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (poi == null) 0 else poi.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: BaseVarPointsTo =>
          (this == other) ||
          (super.equals(other) && (this.poi == null && other.poi == null || this.poi != null && this.poi == other.poi))
        case _ => false
      }
    }
  }

  protected def flowsTo(): Unit = {
    if (active) return
    active = true
    if (isLogEnabled()) {
      LOGGER.trace("POI: Propagation of {} flows to {}", baseSolver, flowSolver)
    }
    handlingAtFieldStatements()
    handlingAtCallSites()
  }

  private def handlingAtFieldStatements(): Unit = {
    baseSolver.registerStatementFieldTransitionListener(
      new ImportIndirectAliases(curr, this.flowSolver, this.baseSolver))
    flowSolver.registerStatementCallTransitionListener(
      new ImportIndirectCallAliases(curr, this.flowSolver))
  }

  private def handlingAtCallSites(): Unit = {
    flowSolver.getCallAutomaton().registerListener(new ForAnyCallSiteOrExitStmt(this.baseSolver))
  }

  private final class ImportIndirectCallAliases(stmt: Edge, var flowSolver: AbstractBoomerangSolver[W]) extends ControlFlowEdgeBasedCallTransitionListener[W](stmt) {

    override def onAddedTransition(t: Transition[Edge, INode[Val]], w: W): Unit = {
      if (t.getStart().fact().equals(storedVar)) {
        baseSolver.registerStatementCallTransitionListener(
          new ImportIndirectCallAliasesAtSucc(curr, t.getTarget(), w))
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ImportIndirectCallAliases =>
          (this == other) ||
          (super.equals(other) && (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)))
        case _ => false
      }
    }
  }

  private final class ImportIndirectCallAliasesAtSucc(succ: Edge, var target: INode[Val], var w: W) extends ControlFlowEdgeBasedCallTransitionListener[W](succ) {

    override def onAddedTransition(t: Transition[Edge, INode[Val]], w: W): Unit = {
      if (getControlFlowEdge().getStart().isFieldStore() && !getControlFlowEdge().getStart().getFieldStore().getX().equals(t.getStart().fact())) {
        flowSolver.getCallAutomaton().addWeightForTransition(new Transition(t.getStart(), t.getLabel(), target), this.w)
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (target == null) 0 else target.hashCode())
      result = prime * result + (if (w == null) 0 else w.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ImportIndirectCallAliasesAtSucc =>
          (this == other) ||
          (super.equals(other) && (this.target == null && other.target == null || this.target != null && this.target.equals(other.target)) &&
          (this.w == null && other.w == null || this.w != null && this.w.equals(other.w)))
        case _ => false
      }
    }
  }

  private final class ImportIndirectAliases(succ: Edge, var flowSolver: AbstractBoomerangSolver[W], var baseSolver: AbstractBoomerangSolver[W]) extends ControlFlowEdgeBasedFieldTransitionListener[W](succ) {

    override def onAddedTransition(t: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
      if (t.getLabel().equals(Field.epsilon())) {
        return
      }
      if (!t.getStart().isInstanceOf[GeneratedState]) {
        importFieldTransitionsStartingAt(t, 0)
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (baseSolver == null) 0 else baseSolver.hashCode())
      result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ImportIndirectAliases =>
          (this == other) ||
          (super.equals(other) && (this.baseSolver == null && other.baseSolver == null || this.baseSolver != null && this.baseSolver.equals(other.baseSolver)) &&
          (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)))
        case _ => false
      }
    }
  }

  private final class CallSiteOrExitStmtFieldImport(flowSolver: AbstractBoomerangSolver[W], baseSolver: AbstractBoomerangSolver[W], reachableNode: Node[Edge, Val]) extends ControlFlowEdgeBasedFieldTransitionListener[W](reachableNode.stmt()) {

    private val fact: Val = reachableNode.fact()

    override def onAddedTransition(innerT: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
      if (innerT.getLabel().equals(Field.epsilon())) {
        return
      }
      if (!innerT.getStart().isInstanceOf[GeneratedState] && innerT.getStart().fact().fact().equals(fact)) {
        importFieldTransitionsStartingAt(innerT, 0)
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (baseSolver == null) 0 else baseSolver.hashCode())
      result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
      result = prime * result + (if (fact == null) 0 else fact.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: CallSiteOrExitStmtFieldImport =>
          (this == other) ||
          (super.equals(other) && (this.baseSolver == null && other.baseSolver == null || this.baseSolver != null && this.baseSolver.equals(other.baseSolver)) &&
          (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)) &&
          (this.fact == null && other.fact == null || this.fact != null && this.fact.equals(other.fact)))
        case _ => false
      }
    }
  }

  protected def importFieldTransitionsStartingAt(t: Transition[Field, INode[Node[Edge, Val]]], importDepth: Int): Unit = {
    if (MAX_IMPORT_DEPTH > 0 && importDepth > MAX_IMPORT_DEPTH) return
    if (t.getLabel().equals(Field.epsilon())) {
      return
    }
    if (t.getLabel().equals(Field.empty())) {
      if (isLogEnabled()) {
        LOGGER.trace("Activating with {}", t.getStart())
      }
      if (baseSolver.getFieldAutomaton().isUnbalancedState(t.getTarget())) {
        activate(t.getStart())
      }
    } else if (t.getTarget().isInstanceOf[GeneratedState]) {
      if (isLogEnabled()) {
        LOGGER.trace("Copying {} into Field Automaton {}", t, flowSolver)
      }
      queueOrAdd(t)
      val newDepth = importDepth + 1
      baseSolver
        .getFieldAutomaton()
        .registerListener(
          new ImportFieldTransitionsFrom(t.getTarget(), this.flowSolver, newDepth))
    }
  }

  def addReachable(node: INode[Node[Edge, Val]]): Unit = {
    if (reachable.add(node)) {
      for (callback <- Lists.newArrayList(delayedTransitions.get(node))) {
        callback.trigger()
      }
    }
  }

  private def queueOrAdd(transToInsert: Transition[Field, INode[Node[Edge, Val]]]): Unit = {
    if (reachable.contains(transToInsert.getTarget())) {
      flowSolver.getFieldAutomaton().addTransition(transToInsert)
      addReachable(transToInsert.getStart())
    } else {
      delayedTransitions.put(
        transToInsert.getTarget(), new InsertFieldTransitionCallback(transToInsert))
    }
  }

  def activate(start: INode[Node[Edge, Val]]): Unit

  def trigger(start: INode[Node[Edge, Val]]): Unit = {
    val intermediateState = flowSolver.getFieldAutomaton().createState(new SingleNode[Node[Edge, Val]](new Node[Edge, Val](curr, baseVar)), field)
    val connectingTrans = new Transition[Field, INode[Node[Edge, Val]]](start, field, intermediateState)
    if (isLogEnabled()) {
      LOGGER.trace("Connecting {} into Field Automaton {}", connectingTrans, flowSolver)
    }
    flowSolver.getFieldAutomaton().addTransition(connectingTrans)
    addReachable(connectingTrans.getStart())
  }

  private final class ImportFieldTransitionsFrom(target: INode[Node[Edge, Val]], var flowSolver: AbstractBoomerangSolver[W], var importDepth: Int) extends WPAStateListener[Field, INode[Node[Edge, Val]], W](target) {

    override def onOutTransitionAdded(t: Transition[Field, INode[Node[Edge, Val]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {
      if (t.getLabel().equals(Field.epsilon())) return
      importFieldTransitionsStartingAt(t, importDepth)
    }

    override def onInTransitionAdded(t: Transition[Field, INode[Node[Edge, Val]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ImportFieldTransitionsFrom =>
          (this == other) ||
          (super.equals(other) && (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)))
        case _ => false
      }
    }
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (baseSolver == null) 0 else baseSolver.hashCode())
    result = prime * result + (if (flowSolver == null) 0 else flowSolver.hashCode())
    result = prime * result + (if (curr == null) 0 else curr.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: ExecuteImportFieldStmtPOI =>
        (this == other) ||
        (this.baseSolver == null && other.baseSolver == null || this.baseSolver != null && this.baseSolver.equals(other.baseSolver)) &&
        (this.flowSolver == null && other.flowSolver == null || this.flowSolver != null && this.flowSolver.equals(other.flowSolver)) &&
        (this.curr == null && other.curr == null || this.curr != null && this.curr.equals(other.curr))
      case _ => false
    }
  }

  private final class InsertFieldTransitionCallback(var trans: Transition[Field, INode[Node[Edge, Val]]]) {

    def trigger(): Unit = {
      flowSolver.getFieldAutomaton().addTransition(trans)
      addReachable(trans.getStart())
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (trans == null) 0 else trans.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: InsertFieldTransitionCallback =>
          (this == other) ||
          (this.trans == null && other.trans == null || this.trans != null && this.trans.equals(other.trans))
        case _ => false
      }
    }
  }
}
