package boomerang.weights

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import boomerang.weights.PathConditionWeight.ConditionDomain
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node

class PathTrackingWeightFunctions(private var trackDataFlowPath: Boolean, private var trackPathConditions: Boolean, private var implicitBooleanCondition: Boolean) extends Nothing {
  @Override def push(curr: Nothing, succ: Nothing, callSite: Nothing): Nothing = {
    if (trackDataFlowPath && !curr.fact.isStatic) {
      if (callSite.getStart.uses(curr.fact)) {
        if (implicitBooleanCondition && callSite.getTarget.isAssign) return new Nothing(new Nothing(callSite, curr.fact), callSite.getStart, succ.stmt.getMethod)
        return new Nothing(new Nothing(callSite, curr.fact))
      }
      if (implicitBooleanCondition && callSite.getStart.isAssign) return new Nothing(callSite.getStart, succ.stmt.getMethod)
    }
    DataFlowPathWeight.one
  }

  @Override def normal(curr: Nothing, succ: Nothing): Nothing = {
    if (trackDataFlowPath && curr.stmt.getMethod.getControlFlowGraph.getStartPoints.contains(curr.stmt)) return new Nothing(curr)
    if (trackDataFlowPath && !curr.fact.equals(succ.fact)) return new Nothing(succ)
    if (trackDataFlowPath && succ.stmt.getTarget.isReturnStmt && succ.stmt.getTarget.getReturnOp.equals(succ.fact)) return new Nothing(succ)
    if (implicitBooleanCondition && curr.stmt.getTarget.isAssign && curr.stmt.getTarget.getLeftOp.getType.isBooleanType) return new Nothing(curr.stmt.getTarget.getLeftOp, if (curr.stmt.getTarget.getRightOp.toString.equals("0")) ConditionDomain.FALSE
    else ConditionDomain.TRUE)
    if (implicitBooleanCondition && succ.stmt.getTarget.isReturnStmt) return new Nothing(succ.stmt.getTarget.getReturnOp)
    if (trackPathConditions && curr.stmt.getTarget.isIfStmt) {
      if (curr.stmt.getTarget.getIfStmt.getTarget.equals(succ.stmt)) return new Nothing(curr.stmt.getTarget, true)
      return new Nothing(curr.stmt.getTarget, false)
    }
    DataFlowPathWeight.one
  }

  @Override def pop(curr: Nothing): Nothing = DataFlowPathWeight.one

  @Override def getOne: Nothing = DataFlowPathWeight.one
}