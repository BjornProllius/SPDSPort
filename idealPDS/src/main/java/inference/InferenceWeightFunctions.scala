package inference

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Val
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node

class InferenceWeightFunctions extends WeightFunctions[Edge, Val, Edge, InferenceWeight] {
  override def push(curr: Node[Edge, Val], succ: Node[Edge, Val], field: Edge): InferenceWeight = {
    val callee = succ.stmt().getMethod()
    if (!callee.isStatic()) {
      val thisLocal = callee.getThisLocal()
      if (succ.fact() == thisLocal) {
        return new InferenceWeight(callee)
      }
    }
    getOne()
  }

  override def normal(curr: Node[Edge, Val], succ: Node[Edge, Val]): InferenceWeight = getOne()

  override def pop(curr: Node[Edge, Val]): InferenceWeight = getOne()

  override def getOne(): InferenceWeight = InferenceWeight.one()
}