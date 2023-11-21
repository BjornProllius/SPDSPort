package boomerang.debugger

import boomerang.Query
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import wpds.impl.Weight

object ConsoleDebugger {
  private val logger = LoggerFactory.getLogger(classOf[ConsoleDebugger[_ <: Nothing]])
}

class ConsoleDebugger[W <: Weight] extends Nothing {
  def done(queryToSolvers: Nothing): Unit = {
    var totalRules = 0
    import scala.collection.JavaConversions._
    for (q <- queryToSolvers.keySet) {
      totalRules += queryToSolvers.get(q).getNumberOfRules
    }
    ConsoleDebugger.logger.debug("Total number of rules: " + totalRules)
    import scala.collection.JavaConversions._
    for (q <- queryToSolvers.keySet) {
      ConsoleDebugger.logger.debug("========================")
      ConsoleDebugger.logger.debug(q.toString)
      ConsoleDebugger.logger.debug("========================")
      queryToSolvers.get(q).debugOutput
      //            for (Method m : queryToSolvers.get(q).getReachableMethods()) {
      //                logger.debug(m + "\n" +
      // Joiner.on("\n\t").join(queryToSolvers.get(q).getResults(m).cellSet()));
      //            }
      queryToSolvers.get(q).debugOutput
    }
  }
}