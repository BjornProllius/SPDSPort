package boomerang.weights

import boomerang.scene.Statement
import boomerang.scene.Val
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node

class MinDistanceWeightFunctions extends Nothing {
  @Override def push(curr: Nothing, succ: Nothing, callSite: Nothing): Nothing = {
    if (!curr.fact.isStatic) return new Nothing(new Nothing(1))
    MinDistanceWeight.one
  }

  @Override def normal(curr: Nothing, succ: Nothing): Nothing = {
    if (!curr.fact.equals(succ.fact)) return new Nothing(new Nothing(1))
    if (succ.stmt.containsInvokeExpr && succ.stmt.uses(curr.fact)) return new Nothing(new Nothing(1))
    MinDistanceWeight.one
  }

  @Override def pop(curr: Nothing): Nothing = MinDistanceWeight.one

  @Override def getOne: Nothing = MinDistanceWeight.one
}