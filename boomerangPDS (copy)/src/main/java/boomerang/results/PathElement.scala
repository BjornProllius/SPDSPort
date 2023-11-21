package boomerang.results

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import java.util.Objects

class PathElement(private val s: Nothing, private val `val`: Nothing, private val stepIndex: Int) {
  def getEdge: Nothing = s

  def getVariable: Nothing = `val`

  def stepIndex: Int = stepIndex

  @Override def equals(o: Nothing): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[PathElement]
    stepIndex == that.stepIndex && Objects.equals(s, that.s) && Objects.equals(`val`, that.`val`)
  }

  @Override def hashCode: Int = Objects.hash(s, `val`, stepIndex)

  @Override def toString: Nothing = String.format("'%d: %s'", stepIndex, getEdge)
}