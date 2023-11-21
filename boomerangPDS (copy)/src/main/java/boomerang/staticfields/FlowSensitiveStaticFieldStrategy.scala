package boomerang.staticfields

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight
import wpds.interfaces.State

class FlowSensitiveStaticFieldStrategy[W <: Weight] extends Nothing {
  @Override def handleForward(storeStmt: Nothing, storedVal: Nothing, staticVal: Nothing, out: Nothing): Unit = {
    out.add(new Nothing(storeStmt, staticVal))
  }

  @Override def handleBackward(loadStatement: Nothing, loadedVal: Nothing, staticVal: Nothing, out: Nothing): Unit = {
    out.add(new Nothing(loadStatement, loadStatement.getTarget.getStaticField))
  }
}