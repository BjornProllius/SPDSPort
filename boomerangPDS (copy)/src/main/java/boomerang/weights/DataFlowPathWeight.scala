package boomerang.weights

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.weights.PathConditionWeight.ConditionDomain
import com.google.common.base.Objects
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

object DataFlowPathWeight {
  private var one: DataFlowPathWeight = null

  def one: DataFlowPathWeight = {
    if (one == null) one = new DataFlowPathWeight
    one
  }
}

class DataFlowPathWeight private extends Nothing {
  path = PathTrackingWeight.one
  condition = PathConditionWeight.one
  private var path: Nothing = null
  private var condition: Nothing = null

  def this(path: Nothing) {
    this()
    this.path = new Nothing(path)
    this.condition = PathConditionWeight.one
  }

  def this(path: Nothing, callSite: Nothing, callee: Nothing) {
    this()
    this.path = new Nothing(path)
    this.condition = new Nothing(callSite, callee)
  }

  def this(callSite: Nothing, callee: Nothing) {
    this()
    this.path = PathTrackingWeight.one
    this.condition = new Nothing(callSite, callee)
  }

  def this(ifStatement: Nothing, condition: Nothing) {
    this()
    this.path = PathTrackingWeight.one
    this.condition = new Nothing(ifStatement, condition)
  }

  def this(path: Nothing, condition: Nothing) {
    this()
    this.path = path
    this.condition = condition
  }

  def this(leftOp: Nothing, conditionVal: Nothing) {
    this()
    this.path = PathTrackingWeight.one
    this.condition = new Nothing(leftOp, conditionVal)
  }

  def this(returnVal: Nothing) {
    this()
    this.path = PathTrackingWeight.one
    this.condition = new Nothing(returnVal)
  }

  @Override def equals(o: Nothing): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[DataFlowPathWeight]
    Objects.equal(path, that.path) && Objects.equal(condition, that.condition)
  }

  @Override def hashCode: Int = Objects.hashCode(path, condition)

  def getAllStatements: Nothing = path.getShortestPathWitness

  def getConditions: Nothing = condition.getConditions

  def getEvaluationMap: Nothing = condition.getEvaluationMap

  @Override def toString: Nothing = " COND: " + condition

  def extendWith(other: Nothing) = new DataFlowPathWeight(path.extendWith(other.asInstanceOf[DataFlowPathWeight].path).asInstanceOf[Nothing], condition.extendWith(other.asInstanceOf[DataFlowPathWeight].condition).asInstanceOf[Nothing])

  @Override def combineWith(other: Nothing) = new DataFlowPathWeight(path.combineWith(other.asInstanceOf[DataFlowPathWeight].path).asInstanceOf[Nothing], condition.combineWith(other.asInstanceOf[DataFlowPathWeight].condition).asInstanceOf[Nothing])
}