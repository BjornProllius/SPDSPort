package boomerang.staticfields

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import java.util
import wpds.impl.Weight
import wpds.interfaces.State

trait StaticFieldStrategy[W <: Weight] {
  def handleForward(storeStmt: Nothing, storedVal: Nothing, staticVal: Nothing, out: Nothing): Unit

  def handleBackward(curr: Nothing, leftOp: Nothing, staticField: Nothing, out: Nothing): Unit
}