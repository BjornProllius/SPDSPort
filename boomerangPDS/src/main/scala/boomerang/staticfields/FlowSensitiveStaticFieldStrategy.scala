package boomerang.staticfields

import boomerang.scene.{ControlFlowGraph, StaticFieldVal, Val}
import sync.pds.solver.nodes.Node
import wpds.impl.Weight
import wpds.interfaces.State

class FlowSensitiveStaticFieldStrategy[W <: Weight] extends StaticFieldStrategy[W] {
  override def handleForward(
      storeStmt: ControlFlowGraph.Edge, 
      storedVal: Val, 
      staticVal: StaticFieldVal, 
      out: Set[State]): Unit = {
    out.add(new Node[ControlFlowGraph.Edge, Val](storeStmt, staticVal))
  }

  override def handleBackward(
      loadStatement: ControlFlowGraph.Edge, 
      loadedVal: Val, 
      staticVal: StaticFieldVal, 
      out: Set[State]): Unit = {
    out.add(new Node[ControlFlowGraph.Edge, Val](loadStatement, loadStatement.getTarget.getStaticField))
  }
}