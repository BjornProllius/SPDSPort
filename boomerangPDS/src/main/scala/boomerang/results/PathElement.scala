package boomerang.results

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val

case class PathElement(s: Edge, val: Val, stepIndex: Int) {

  def getEdge(): Edge = s

  def getVariable(): Val = val

  def stepIndex(): Int = stepIndex

  override def equals(o: Any): Boolean = o match {
    case that: PathElement => stepIndex == that.stepIndex && (s == that.s) && (val == that.val)
    case _ => false
  }

  override def hashCode(): Int = (s, val, stepIndex).##

  override def toString(): String = s"'$stepIndex: $getEdge'"
}