package boomerang.weights

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{Method, Statement, Val}
import boomerang.weights.PathConditionWeight.ConditionDomain
import com.google.common.base.Objects
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

class DataFlowPathWeight private (path: PathTrackingWeight, condition: PathConditionWeight) extends Weight {

  def this() {
    this(PathTrackingWeight.one(), PathConditionWeight.one())
  }

  def this(path: Node[Edge, Val]) {
    this(new PathTrackingWeight(path), PathConditionWeight.one())
  }

  def this(path: Node[Edge, Val], callSite: Statement, callee: Method) {
    this(new PathTrackingWeight(path), new PathConditionWeight(callSite, callee))
  }

  def this(callSite: Statement, callee: Method) {
    this(PathTrackingWeight.one(), new PathConditionWeight(callSite, callee))
  }

  def this(ifStatement: Statement, condition: Boolean) {
    this(PathTrackingWeight.one(), new PathConditionWeight(ifStatement, condition))
  }

  def this(leftOp: Val, conditionVal: ConditionDomain) {
    this(PathTrackingWeight.one(), new PathConditionWeight(leftOp, conditionVal))
  }

  def this(returnVal: Val) {
    this(PathTrackingWeight.one(), new PathConditionWeight(returnVal))
  }

  override def equals(o: Any): Boolean = o match {
    case that: DataFlowPathWeight => Objects.equal(path, that.path) && Objects.equal(condition, that.condition)
    case _ => false
  }

  override def hashCode: Int = Objects.hashCode(path, condition)

  def getAllStatements: List[Node[Edge, Val]] = path.getShortestPathWitness

  def getConditions: Map[Statement, ConditionDomain] = condition.getConditions

  def getEvaluationMap: Map[Val, ConditionDomain] = condition.getEvaluationMap

  override def toString: String = " COND: " + condition

  def extendWith(other: Weight): Weight = {
    new DataFlowPathWeight(
      path.extendWith(other.asInstanceOf[DataFlowPathWeight].path).asInstanceOf[PathTrackingWeight],
      condition.extendWith(other.asInstanceOf[DataFlowPathWeight].condition).asInstanceOf[PathConditionWeight]
    )
  }

  override def combineWith(other: Weight): Weight = {
    new DataFlowPathWeight(
      path.combineWith(other.asInstanceOf[DataFlowPathWeight].path).asInstanceOf[PathTrackingWeight],
      condition.combineWith(other.asInstanceOf[DataFlowPathWeight].condition).asInstanceOf[PathConditionWeight]
    )
  }
}

object DataFlowPathWeight {
  private var one: DataFlowPathWeight = _

  def one(): DataFlowPathWeight = {
    if (one == null) one = new DataFlowPathWeight()
    one
  }
}