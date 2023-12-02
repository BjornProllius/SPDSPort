package boomerang.staticfields

import boomerang.scene.{ControlFlowGraph, StaticFieldVal, Val}
import wpds.impl.Weight
import wpds.interfaces.State

class IgnoreStaticFieldStrategy[W <: Weight] extends StaticFieldStrategy[W] {
  override def handleForward(
      storeStmt: ControlFlowGraph.Edge, 
      storedVal: Val, 
      staticVal: StaticFieldVal, 
      out: Set[State]): Unit = {}

  override def handleBackward(
      loadStatement: ControlFlowGraph.Edge, 
      loadedVal: Val, 
      staticVal: StaticFieldVal, 
      out: Set[State]): Unit = {}
}