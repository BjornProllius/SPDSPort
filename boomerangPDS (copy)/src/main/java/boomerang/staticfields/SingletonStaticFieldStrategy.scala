package boomerang.staticfields

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import com.google.common.collect.Multimap
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight
import wpds.interfaces.State

class SingletonStaticFieldStrategy[W <: Weight](private var solver: Nothing, private var fieldLoadStatements: Nothing, private var fieldStoreStatements: Nothing) extends Nothing {
  @Override def handleForward(storeStmt: Nothing, storedVal: Nothing, staticVal: Nothing, out: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (matchingStore <- fieldLoadStatements.get(staticVal.field)) {
      if (matchingStore.isAssign) {
        import scala.collection.JavaConversions._
        for (succ <- matchingStore.getMethod.getControlFlowGraph.getSuccsOf(matchingStore)) {
          solver.processNormal(new Nothing(storeStmt, storedVal), new Nothing(new Nothing(matchingStore, succ), matchingStore.getLeftOp))
        }
      }
    }
  }

  @Override def handleBackward(loadStatement: Nothing, loadedVal: Nothing, staticVal: Nothing, out: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (matchingStore <- fieldStoreStatements.get(staticVal.field)) {
      import scala.collection.JavaConversions._
      for (pred <- matchingStore.getMethod.getControlFlowGraph.getPredsOf(matchingStore)) {
        solver.processNormal(new Nothing(loadStatement, loadedVal), new Nothing(new Nothing(pred, matchingStore), matchingStore.getRightOp))
      }
    }
  }
}