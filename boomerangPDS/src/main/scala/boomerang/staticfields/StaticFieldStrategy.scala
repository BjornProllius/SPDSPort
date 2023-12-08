package boomerang.staticfields

import boomerang.scene.{ControlFlowGraph, StaticFieldVal, Val}
import wpds.impl.Weight
import wpds.interfaces.State

trait StaticFieldStrategy[W <: Weight] {
  def handleForward(
    storeStmt: ControlFlowGraph.Edge, 
    storedVal: Val, 
    staticVal: StaticFieldVal, 
    out: Set[State]): Unit

  def handleBackward(
    curr: ControlFlowGraph.Edge, 
    leftOp: Val, 
    staticField: StaticFieldVal, 
    out: Set[State]): Unit
}