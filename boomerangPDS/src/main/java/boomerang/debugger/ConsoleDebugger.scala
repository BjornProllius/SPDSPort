package boomerang.debugger

import boomerang.{ForwardQuery, Query}
import boomerang.solver.ForwardBoomerangSolver
import org.slf4j.{Logger, LoggerFactory}
import wpds.impl.Weight

import scala.collection.JavaConverters._

class ConsoleDebugger[W <: Weight] extends Debugger[W] {
  private val logger: Logger = LoggerFactory.getLogger(classOf[ConsoleDebugger[_]])

  def done(queryToSolvers: java.util.Map[ForwardQuery, ForwardBoomerangSolver[W]]): Unit = {
    var totalRules = 0
    for (q <- queryToSolvers.keySet().asScala) {
      totalRules += queryToSolvers.get(q).getNumberOfRules()
    }
    logger.debug("Total number of rules: " + totalRules)
    for (q <- queryToSolvers.keySet().asScala) {
      logger.debug("========================")
      logger.debug(q.toString)
      logger.debug("========================")
      queryToSolvers.get(q).debugOutput()
      // for (m <- queryToSolvers.get(q).getReachableMethods()) {
      //   logger.debug(m + "\n" +
      //   Joiner.on("\n\t").join(queryToSolvers.get(q).getResults(m).cellSet()))
      // }
      queryToSolvers.get(q).debugOutput()
    }
  }
}