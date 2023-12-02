package boomerang.weights

import boomerang.scene.{Statement, Val}
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node

class MinDistanceWeightFunctions extends WeightFunctions[Statement, Val, Statement, MinDistanceWeight] {

  override def push(curr: Node[Statement, Val], succ: Node[Statement, Val], callSite: Statement): MinDistanceWeight = {
    if (!curr.fact().isStatic()) {
      new MinDistanceWeight(1)
    } else {
      MinDistanceWeight.one
    }
  }

  override def normal(curr: Node[Statement, Val], succ: Node[Statement, Val]): MinDistanceWeight = {
    if (!curr.fact().equals(succ.fact())) {
      new MinDistanceWeight(1)
    } else if (succ.stmt().containsInvokeExpr() && succ.stmt().uses(curr.fact())) {
      new MinDistanceWeight(1)
    } else {
      MinDistanceWeight.one
    }
  }

  override def pop(curr: Node[Statement, Val]): MinDistanceWeight = {
    MinDistanceWeight.one
  }

  override def getOne: MinDistanceWeight = {
    MinDistanceWeight.one
  }
}