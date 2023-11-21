package boomerang.poi

import boomerang.BackwardQuery
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.BackwardBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.base.Objects
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener

object CopyAccessPathChain {
  private val MAX_WALK_DEPTH = -1
}

class CopyAccessPathChain[W <: Weight](private var forwardSolver: Nothing, private var backwardSolver: Nothing, private var fieldWriteStatement: Nothing, killedTransition: Nothing) {
  this.killedTransitionTarget = killedTransition.getTarget
  // TODO: Source of non-determinsim: not part of hashCode/equals....but also shall not be.
  private var killedTransitionTarget: Nothing = null

  def exec(): Unit = {
    forwardSolver.getFieldAutomaton.registerListener(new CopyAccessPathChain[W]#WalkForwardSolverListener(killedTransitionTarget, new Nothing(new Nothing(fieldWriteStatement, fieldWriteStatement.getTarget.getRightOp)), 0))
  }

  private class WalkForwardSolverListener(target: Nothing, private var stateInBwSolver: Nothing, private var walkDepth: Int) extends Nothing(target) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(Field.empty)) {
        if (forwardSolver.getFieldAutomaton.isUnbalancedState(t.getTarget)) if (t.getStart.equals(thisCopyAccessPathChain.killedTransitionTarget)) {
          // Do a simple backwardSolve(...)...
          val query = BackwardQuery.make(fieldWriteStatement, fieldWriteStatement.getTarget.getRightOp)
          val fieldTarget = backwardSolver.createQueryNodeField(query)
          val callTarget = backwardSolver.generateCallState(new Nothing(query.`var`), query.cfgEdge)
          backwardSolver.solve(query.asNode, Field.empty, fieldTarget, query.cfgEdge, callTarget)
          return
        }
        return
      }
      val targetState = backwardSolver.generateFieldState(new Nothing(new Nothing(new Nothing(Statement.epsilon, Statement.epsilon), Val.zero)), t.getLabel)
      val insert = new Nothing(stateInBwSolver, t.getLabel, targetState)
      queueOrAdd(insert)
      val newDepth = walkDepth + 1
      if (CopyAccessPathChain.MAX_WALK_DEPTH < 0 || newDepth < CopyAccessPathChain.MAX_WALK_DEPTH) forwardSolver.getFieldAutomaton.registerListener(new CopyAccessPathChain[W]#WalkForwardSolverListener(t.getTarget, targetState, newDepth))
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def equals(o: Nothing): Boolean = {
      if (this eq o) return true
      if (o == null || (getClass ne o.getClass)) return false
      val that = o.asInstanceOf[CopyAccessPathChain[W]#WalkForwardSolverListener]
      if (!(getEnclosingInstance == that.getEnclosingInstance)) return false
      Objects.equal(stateInBwSolver, that.stateInBwSolver)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (stateInBwSolver == null) 0
      else stateInBwSolver.hashCode)
      result
    }

    private def getEnclosingInstance = thisCopyAccessPathChain
  }

  // Copied from ExecuteImportFielStmtPOI
  private val reachable = Sets.newHashSet
  private val delayedTransitions = HashMultimap.create

  def addReachable(node: Nothing): Unit = {
    if (reachable.add(node)) {
      import scala.collection.JavaConversions._
      for (callback <- Lists.newArrayList(delayedTransitions.get(node))) {
        callback.trigger()
      }
    }
  }

  private def queueOrAdd(transToInsert: Nothing): Unit = {
    if (reachable.contains(transToInsert.getTarget)) backwardSolver.getFieldAutomaton.addTransition(transToInsert)
    else delayedTransitions.put(transToInsert.getTarget, new CopyAccessPathChain[W]#InsertFieldTransitionCallback(transToInsert))
  }

  private class InsertFieldTransitionCallback(private val trans: Nothing) {
    def trigger(): Unit = {
      backwardSolver.getFieldAutomaton.addTransition(trans)
      addReachable(trans.getStart)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (trans == null) 0
      else trans.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[CopyAccessPathChain[W]#InsertFieldTransitionCallback]
      if (!(getEnclosingInstance == other.getEnclosingInstance)) return false
      if (trans == null) if (other.trans != null) return false
      else if (!trans.equals(other.trans)) return false
      true
    }

    private def getEnclosingInstance = thisCopyAccessPathChain
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (backwardSolver == null) 0
    else backwardSolver.hashCode)
    result = prime * result + (if (fieldWriteStatement == null) 0
    else fieldWriteStatement.hashCode)
    result = prime * result + (if (forwardSolver == null) 0
    else forwardSolver.hashCode)
    result = prime * result + (if (killedTransitionTarget == null) 0
    else killedTransitionTarget.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[CopyAccessPathChain[_ <: Nothing]]
    if (backwardSolver == null) if (other.backwardSolver != null) return false
    else if (!backwardSolver.equals(other.backwardSolver)) return false
    if (fieldWriteStatement == null) if (other.fieldWriteStatement != null) return false
    else if (!fieldWriteStatement.equals(other.fieldWriteStatement)) return false
    if (forwardSolver == null) if (other.forwardSolver != null) return false
    else if (!forwardSolver.equals(other.forwardSolver)) return false
    if (killedTransitionTarget == null) if (other.killedTransitionTarget != null) return false
    else if (!killedTransitionTarget.equals(other.killedTransitionTarget)) return false
    true
  }
}