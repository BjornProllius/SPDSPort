package boomerang.staticfields

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import java.util
import wpds.impl.Weight
import wpds.interfaces.State

class IgnoreStaticFieldStrategy[W <: Weight] extends Nothing {
  @Override def handleForward(storeStmt: Nothing, storedVal: Nothing, staticVal: Nothing, out: Nothing): Unit = {
  }

  @Override def handleBackward(loadStatement: Nothing, loadedVal: Nothing, staticVal: Nothing, out: Nothing): Unit = {
  }
}