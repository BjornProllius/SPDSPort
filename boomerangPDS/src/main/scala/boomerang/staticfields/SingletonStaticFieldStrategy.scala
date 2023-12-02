package boomerang.staticfields

import boomerang.scene.{ControlFlowGraph, Field, Statement, StaticFieldVal, Val}
import boomerang.solver.AbstractBoomerangSolver
import com.google.common.collect.Multimap
import sync.pds.solver.nodes.Node
import wpds.impl.{Edge, Weight}
import wpds.interfaces.State

class SingletonStaticFieldStrategy[W <: Weight](
    var solver: AbstractBoomerangSolver,
    var fieldLoadStatements: Multimap[Field, Statement],
    var fieldStoreStatements: Multimap[Field, Statement]
) extends StaticFieldStrategy[W] {

  override def handleForward(
      storeStmt: Edge, 
      storedVal: Val, 
      staticVal: StaticFieldVal, 
      out: Set[State]): Unit = {
    for (matchingStore <- fieldLoadStatements.get(staticVal.field())) {
      if (matchingStore.isAssign()) {
        for (succ <- matchingStore.getMethod().getControlFlowGraph().getSuccsOf(matchingStore)) {
          solver.processNormal(
            new Node[Edge, Val](storeStmt, storedVal),
            new Node[Edge, Val](new Edge(matchingStore, succ), matchingStore.getLeftOp())
          )
        }
      }
    }
  }

  override def handleBackward(
      loadStatement: Edge, 
      loadedVal: Val, 
      staticVal: StaticFieldVal, 
      out: Set[State]): Unit = {
    for (matchingStore <- fieldStoreStatements.get(staticVal.field())) {
      for (pred <- matchingStore.getMethod().getControlFlowGraph().getPredsOf(matchingStore)) {
        solver.processNormal(
          new Node[Edge, Val](loadStatement, loadedVal),
          new Node[Edge, Val](new Edge(pred, matchingStore), matchingStore.getRightOp())
        )
      }
    }
  }
}