package boomerang.scene.jimple

import boomerang.scene.CallGraph
import boomerang.scene.Statement
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import soot.Scene
import soot.SootMethod

class SootCallGraph extends Nothing {
  val callGraph: Nothing = Scene.v.getCallGraph

  import scala.collection.JavaConversions._

  for (e <- callGraph) {
    if (e.src.hasActiveBody && e.tgt.hasActiveBody && e.srcStmt != null) {
      val callSite = JimpleStatement.create(e.srcStmt, JimpleMethod.of(e.src))
      if (callSite.containsInvokeExpr) {
        LOGGER.trace("Call edge from {} to target method {}", callSite, e.tgt)
        this.addEdge(new Nothing(callSite, JimpleMethod.of(e.tgt)))
      }
    }
  }
  import scala.collection.JavaConversions._

  for (m <- Scene.v.getEntryPoints) {
    if (method.hasActiveBody) this.addEntryPoint(JimpleMethod.of(method))
  }
  private[jimple] val LOGGER = LoggerFactory.getLogger(classOf[SootCallGraph])
}