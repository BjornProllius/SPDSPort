package boomerang.weights

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node

class PathTrackingWeightFunctions(var trackDataFlowPath: Boolean, var trackPathConditions: Boolean, var implicitBooleanCondition: Boolean)
  extends WeightFunctions[Edge, Val, Edge, DataFlowPathWeight] {

  override def push(curr: Node[Edge, Val], succ: Node[Edge, Val], callSite: Edge): DataFlowPathWeight = {
    if (trackDataFlowPath && !curr.fact().isStatic()) {
      if (callSite.getStart.uses(curr.fact())) {
        if (implicitBooleanCondition && callSite.getTarget.isAssign) {
          return new DataFlowPathWeight(new Node[Edge, Val](callSite, curr.fact()), callSite.getStart, succ.stmt().getMethod)
        }
        return new DataFlowPathWeight(new Node[Edge, Val](callSite, curr.fact()))
      }
      if (implicitBooleanCondition && callSite.getStart.isAssign) {
        return new DataFlowPathWeight(callSite.getStart, succ.stmt().getMethod)
      }
    }
    DataFlowPathWeight.one
  }

  override def normal(curr: Node[Edge, Val], succ: Node[Edge, Val]): DataFlowPathWeight = {
    if (trackDataFlowPath && curr.stmt().getMethod.getControlFlowGraph.getStartPoints.contains(curr.stmt())) {
      return new DataFlowPathWeight(curr)
    }
    if (trackDataFlowPath && !curr.fact().equals(succ.fact())) {
      return new DataFlowPathWeight(succ)
    }
    if (trackDataFlowPath && succ.stmt().getTarget.isReturnStmt && succ.stmt().getTarget.getReturnOp.equals(succ.fact())) {
      return new DataFlowPathWeight(succ)
    }
    if (implicitBooleanCondition && curr.stmt().getTarget.isAssign && curr.stmt().getTarget.getLeftOp.getType.isBooleanType) {
      return new DataFlowPathWeight(
        curr.stmt().getTarget.getLeftOp,
        if (curr.stmt().getTarget.getRightOp.toString.equals("0")) ConditionDomain.FALSE else ConditionDomain.TRUE
      )
    }
    if (implicitBooleanCondition && succ.stmt().getTarget.isReturnStmt) {
      return new DataFlowPathWeight(succ.stmt().getTarget.getReturnOp)
    }
    if (trackPathConditions && curr.stmt().getTarget.isIfStmt) {
      if (curr.stmt().getTarget.getIfStmt.getTarget.equals(succ.stmt())) {
        return new DataFlowPathWeight(curr.stmt().getTarget, true)
      }
      return new DataFlowPathWeight(curr.stmt().getTarget, false)
    }
    DataFlowPathWeight.one
  }

  override def pop(curr: Node[Edge, Val]): DataFlowPathWeight = {
    DataFlowPathWeight.one
  }

  override def getOne: DataFlowPathWeight = {
    DataFlowPathWeight.one
  }
}