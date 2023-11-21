package boomerang.poi

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ControlFlowEdgeBasedCallTransitionListener
import boomerang.solver.ControlFlowEdgeBasedFieldTransitionListener
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener
import wpds.interfaces.WPAUpdateListener

object ExecuteImportFieldStmtPOI {
  private val LOGGER = LoggerFactory.getLogger(classOf[ExecuteImportFieldStmtPOI[_ <: Nothing]])
  private val MAX_IMPORT_DEPTH = -1
}

abstract class ExecuteImportFieldStmtPOI[W <: Weight](protected val baseSolver: Nothing, protected val flowSolver: Nothing, poi: Nothing) {
  this.baseAutomaton = baseSolver.getFieldAutomaton
  this.flowAutomaton = flowSolver.getFieldAutomaton
  this.strongUpdateNode = poi.getCfgEdge
  this.baseVar = poi.getBaseVar
  this.storedVar = poi.getStoredVar
  this.field = poi.getField
  private val reachable = Sets.newHashSet
  private val delayedTransitions = HashMultimap.create
  final protected var curr: Nothing = null
  final protected var baseAutomaton: Nothing = null
  final protected var flowAutomaton: Nothing = null
  final private var baseVar: Nothing = null
  final private var storedVar: Nothing = null
  final private var field: Nothing = null
  private[poi] val active = false

  private def isLogEnabled = true

  final private class ImportTransitionFromCall(private var flowSolver: Nothing, stmt: Nothing, private val start: Nothing, private var target: Nothing, private var w: W) extends Nothing(stmt) {
    @Override def onAddedTransition(t: Nothing, w: W): Unit = {
      if (t.getStart.isInstanceOf[Nothing]) return
      val newTrans = new Nothing(t.getStart, t.getLabel, target)
      if (isLogEnabled) ExecuteImportFieldStmtPOI.LOGGER.trace("Copying {} to {}", newTrans, flowSolver)
      if (!t.getStart.equals(start)) if (t.getStart.fact.m.equals(t.getLabel.getStart.getMethod)) {
        // To compute the right Data-Flow Path, apparently, Weight.ONE is necessary and the
        // following line works.
        // flowSolver.getCallAutomaton().addTransition(newTrans);
        // For IDEAL the Weight this.w must be carried along:
        // FileMustBeClosedTest.flowViaFieldDirect
        flowSolver.getCallAutomaton.addWeightForTransition(newTrans, this.w)
      }
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result = prime * result + (if (target == null) 0
      else target.hashCode)
      result = prime * result + (if (w == null) 0
      else w.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ImportTransitionFromCall]
      if (flowSolver == null) if (other.flowSolver != null) return false
      else if (!flowSolver.equals(other.flowSolver)) return false
      if (target == null) if (other.target != null) return false
      else if (!target.equals(other.target)) return false
      if (w == null) if (other.w != null) return false
      else if (!w.equals(other.w)) return false
      true
    }
  }

  final private class ImportOnReachStatement private(private var flowSolver: Nothing, callSiteOrExitStmt: Nothing) extends Nothing(callSiteOrExitStmt) {
    @Override def onAddedTransition(t: Nothing, w: W): Unit = {
      if (t.getStart.isInstanceOf[Nothing]) return
      if (t.getLabel.equals(getControlFlowEdge)) baseSolver.registerStatementFieldTransitionListener(new ExecuteImportFieldStmtPOI[W]#CallSiteOrExitStmtFieldImport(flowSolver, baseSolver, new Nothing(t.getLabel, t.getStart.fact)))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ImportOnReachStatement]
      if (flowSolver == null) if (other.flowSolver != null) return false
      else if (!flowSolver.equals(other.flowSolver)) return false
      true
    }
  }

  private class ForAnyCallSiteOrExitStmt(private var baseSolver: Nothing) extends Nothing {
    @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      if (!flowSolver.getCallAutomaton.isUnbalancedState(t.getTarget)) return
      if (t.getLabel.equals(new Nothing(Statement.epsilon, Statement.epsilon))) return
      val edge = t.getLabel
      val callSite = edge.getStart
      /*  if (t.getStart().fact() instanceof AllocVal) {
              if (((AllocVal) t.getStart().fact())
                  .getDelegate()
                  .equals(t.getTarget().fact().asUnbalanced(null))) {
                return;
              }
            } else if (t.getStart()
                .fact()
                .asUnbalanced(null)
                .equals(t.getTarget().fact().asUnbalanced(null))) return;*/
      if (callSite.containsInvokeExpr) {
        if (callSite.isAssign && callSite.getLeftOp.equals(t.getStart.fact)) return
        if (callSite.uses(t.getStart.fact)) importSolvers(edge, t.getStart, t.getTarget, w)
      }
    }

    private def importSolvers(callSiteOrExitStmt: Nothing, start: Nothing, node: Nothing, w: W): Unit = {
      if (isLogEnabled) ExecuteImportFieldStmtPOI.LOGGER.trace("Importing solvers at {} from {} to {}", callSiteOrExitStmt, baseSolver, flowSolver)
      baseSolver.registerStatementCallTransitionListener(new ExecuteImportFieldStmtPOI[W]#ImportOnReachStatement(flowSolver, callSiteOrExitStmt))
      baseSolver.registerStatementCallTransitionListener(new ExecuteImportFieldStmtPOI[W]#ImportTransitionFromCall(flowSolver, callSiteOrExitStmt, start, node, w))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (baseSolver == null) 0
      else baseSolver.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ForAnyCallSiteOrExitStmt]
      if (baseSolver == null) if (other.baseSolver != null) return false
      else if (!baseSolver.equals(other.baseSolver)) return false
      true
    }
  }

  def solve(): Unit = {
    if (baseSolver.equals(flowSolver)) return
    baseSolver.registerStatementFieldTransitionListener(new ExecuteImportFieldStmtPOI[W]#BaseVarPointsTo(curr, this))
  }

  private class BaseVarPointsTo(curr: Nothing, private var poi: ExecuteImportFieldStmtPOI[W]) extends Nothing(curr) {
    @Override def onAddedTransition(t: Nothing): Unit = {
      val aliasedVariableAtStmt = t.getStart
      if (active) return
      if (!aliasedVariableAtStmt.isInstanceOf[Nothing]) {
        val alias = aliasedVariableAtStmt.fact.fact
        if (alias.equals(poi.baseVar) && t.getLabel.equals(Field.empty)) flowsTo()
      }
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (poi == null) 0
      else poi.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#BaseVarPointsTo]
      if (poi == null) if (other.poi != null) return false
      else if (!(poi == other.poi)) return false
      true
    }
  }

  protected def flowsTo(): Unit = {
    if (active) return
    active = true
    if (isLogEnabled) ExecuteImportFieldStmtPOI.LOGGER.trace("POI: Propagation of {} flows to {}", baseSolver, flowSolver)
    handlingAtFieldStatements()
    handlingAtCallSites()
  }

  private def handlingAtFieldStatements(): Unit = {
    baseSolver.registerStatementFieldTransitionListener(new ExecuteImportFieldStmtPOI[W]#ImportIndirectAliases(curr, this.flowSolver, this.baseSolver))
    flowSolver.registerStatementCallTransitionListener(new ExecuteImportFieldStmtPOI[W]#ImportIndirectCallAliases(curr, this.flowSolver))
  }

  private def handlingAtCallSites(): Unit = {
    flowSolver.getCallAutomaton.registerListener(new ExecuteImportFieldStmtPOI[W]#ForAnyCallSiteOrExitStmt(this.baseSolver))
  }

  final private class ImportIndirectCallAliases(stmt: Nothing, private var flowSolver: Nothing) extends Nothing(stmt) {
    @Override def onAddedTransition(t: Nothing, w: W): Unit = {
      if (t.getStart.fact.equals(storedVar)) baseSolver.registerStatementCallTransitionListener(new ExecuteImportFieldStmtPOI[W]#ImportIndirectCallAliasesAtSucc(curr, t.getTarget, w))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ImportIndirectCallAliases]
      if (flowSolver == null) if (other.flowSolver != null) return false
      else if (!flowSolver.equals(other.flowSolver)) return false
      true
    }
  }

  final private class ImportIndirectCallAliasesAtSucc(succ: Nothing, private var target: Nothing, private var w: W) extends Nothing(succ) {
    @Override def onAddedTransition(t: Nothing, w: W): Unit = {
      if (getControlFlowEdge.getStart.isFieldStore && !getControlFlowEdge.getStart.getFieldStore.getX.equals(t.getStart.fact)) flowSolver.getCallAutomaton.addWeightForTransition(new Nothing(t.getStart, t.getLabel, target), this.w)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (target == null) 0
      else target.hashCode)
      result = prime * result + (if (w == null) 0
      else w.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ImportIndirectCallAliasesAtSucc]
      if (target == null) if (other.target != null) return false
      else if (!target.equals(other.target)) return false
      if (w == null) if (other.w != null) return false
      else if (!w.equals(other.w)) return false
      true
    }
  }

  final private class ImportIndirectAliases(succ: Nothing, private var flowSolver: Nothing, private var baseSolver: Nothing) extends Nothing(succ) {
    @Override def onAddedTransition(t: Nothing): Unit = {
      if (t.getLabel.equals(Field.epsilon)) return
      if (!t.getStart.isInstanceOf[Nothing]) importFieldTransitionsStartingAt(t, 0)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (baseSolver == null) 0
      else baseSolver.hashCode)
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ImportIndirectAliases]
      if (baseSolver == null) if (other.baseSolver != null) return false
      else if (!baseSolver.equals(other.baseSolver)) return false
      if (flowSolver == null) if (other.flowSolver != null) return false
      else return flowSolver.equals(other.flowSolver)
      true
    }
  }

  final private class CallSiteOrExitStmtFieldImport private(private var flowSolver: Nothing, private var baseSolver: Nothing, reachableNode: Nothing) extends Nothing(reachableNode.stmt) {
    this.fact = reachableNode.fact
    private var fact: Nothing = null

    @Override def onAddedTransition(innerT: Nothing): Unit = {
      if (innerT.getLabel.equals(Field.epsilon)) return
      if (!innerT.getStart.isInstanceOf[Nothing] && innerT.getStart.fact.fact.equals(fact)) importFieldTransitionsStartingAt(innerT, 0)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (baseSolver == null) 0
      else baseSolver.hashCode)
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result = prime * result + (if (fact == null) 0
      else fact.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#CallSiteOrExitStmtFieldImport]
      if (baseSolver == null) if (other.baseSolver != null) return false
      else if (!baseSolver.equals(other.baseSolver)) return false
      if (flowSolver == null) if (other.flowSolver != null) return false
      else if (!flowSolver.equals(other.flowSolver)) return false
      if (fact == null) if (other.fact != null) return false
      else if (!fact.equals(other.fact)) return false
      true
    }
  }

  protected def importFieldTransitionsStartingAt(t: Nothing, importDepth: Int): Unit = {
    if (ExecuteImportFieldStmtPOI.MAX_IMPORT_DEPTH > 0 && importDepth > ExecuteImportFieldStmtPOI.MAX_IMPORT_DEPTH) return
    if (t.getLabel.equals(Field.epsilon)) return
    if (t.getLabel.equals(Field.empty)) {
      if (isLogEnabled) ExecuteImportFieldStmtPOI.LOGGER.trace("Activating with {}", t.getStart)
      if (baseSolver.getFieldAutomaton.isUnbalancedState(t.getTarget)) activate(t.getStart)
    }
    else if (t.getTarget.isInstanceOf[Nothing]) {
      if (isLogEnabled) ExecuteImportFieldStmtPOI.LOGGER.trace("Copying {} into Field Automaton {}", t, flowSolver)
      queueOrAdd(t)
      val newDepth = importDepth + 1
      baseSolver.getFieldAutomaton.registerListener(new ExecuteImportFieldStmtPOI[W]#ImportFieldTransitionsFrom(t.getTarget, this.flowSolver, newDepth))
    }
  }

  def addReachable(node: Nothing): Unit = {
    if (reachable.add(node)) {
      import scala.collection.JavaConversions._
      for (callback <- Lists.newArrayList(delayedTransitions.get(node))) {
        callback.trigger()
      }
    }
  }

  private def queueOrAdd(transToInsert: Nothing): Unit = {
    if (reachable.contains(transToInsert.getTarget)) {
      flowSolver.getFieldAutomaton.addTransition(transToInsert)
      addReachable(transToInsert.getStart)
    }
    else delayedTransitions.put(transToInsert.getTarget, new ExecuteImportFieldStmtPOI[W]#InsertFieldTransitionCallback(transToInsert))
  }

  def activate(start: Nothing): Unit

  def trigger(start: Nothing): Unit = {
    val intermediateState = flowSolver.getFieldAutomaton.createState(new Nothing(new Nothing(curr, baseVar)), field)
    val connectingTrans = new Nothing(start, field, intermediateState)
    if (isLogEnabled) ExecuteImportFieldStmtPOI.LOGGER.trace("Connecting {} into Field Automaton {}", connectingTrans, flowSolver)
    flowSolver.getFieldAutomaton.addTransition(connectingTrans)
    addReachable(connectingTrans.getStart)
  }

  final private class ImportFieldTransitionsFrom(target: Nothing, private var flowSolver: Nothing, private var importDepth: Int) extends Nothing(target) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(Field.epsilon)) return
      importFieldTransitionsStartingAt(t, importDepth)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (flowSolver == null) 0
      else flowSolver.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#ImportFieldTransitionsFrom]
      if (flowSolver == null) if (other.flowSolver != null) return false
      else if (!flowSolver.equals(other.flowSolver)) return false
      true
    }
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (baseSolver == null) 0
    else baseSolver.hashCode)
    result = prime * result + (if (flowSolver == null) 0
    else flowSolver.hashCode)
    result = prime * result + (if (curr == null) 0
    else curr.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[_ <: Nothing]]
    if (baseSolver == null) if (other.baseSolver != null) return false
    else if (!baseSolver.equals(other.baseSolver)) return false
    if (flowSolver == null) if (other.flowSolver != null) return false
    else if (!flowSolver.equals(other.flowSolver)) return false
    if (curr == null) if (other.curr != null) return false
    else if (!curr.equals(other.curr)) return false
    true
  }

  private class InsertFieldTransitionCallback(private val trans: Nothing) {
    def trigger(): Unit = {
      flowSolver.getFieldAutomaton.addTransition(trans)
      addReachable(trans.getStart)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (trans == null) 0
      else trans.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExecuteImportFieldStmtPOI[W]#InsertFieldTransitionCallback]
      if (trans == null) if (other.trans != null) return false
      else if (!trans.equals(other.trans)) return false
      true
    }
  }
}