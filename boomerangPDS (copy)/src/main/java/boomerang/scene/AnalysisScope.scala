package boomerang.scene

import boomerang.Query
import boomerang.scene.CallGraph.Edge
import com.google.common.base.Stopwatch
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object AnalysisScope {
  private val LOGGER = LoggerFactory.getLogger(classOf[AnalysisScope])
}

abstract class AnalysisScope(private var cg: Nothing) {
  private var scanLibraryClasses = false
  final private val seeds = Sets.newHashSet
  private val processed = Sets.newHashSet
  private var statementCount = 0

  def setScanLibraryClasses(enabled: Boolean): Unit = {
    scanLibraryClasses = enabled
  }

  def computeSeeds: Nothing = {
    val entryPoints = cg.getEntryPoints
    AnalysisScope.LOGGER.info("Computing seeds starting at {} entry method(s).", entryPoints.size)
    val watch = Stopwatch.createStarted
    val worklist = Lists.newLinkedList
    worklist.addAll(entryPoints)
    while (!worklist.isEmpty) {
      val m = worklist.pop
      if (!processed.add(m)) continue //todo: continue is not supported
      AnalysisScope.LOGGER.trace("Processing {}", m)
      import scala.collection.JavaConversions._
      for (u <- m.getStatements) {
        statementCount += 1
        if (u.containsInvokeExpr) {
          val edgesOutOf = cg.edgesOutOf(u)
          import scala.collection.JavaConversions._
          for (e <- edgesOutOf) {
            val tgt = e.tgt
            if (!scanLibraryClasses && !tgt.getDeclaringClass.isApplicationClass) continue //todo: continue is not supported
            if (!processed.contains(tgt)) worklist.add(tgt)
          }
        }
        import scala.collection.JavaConversions._
        for (succ <- u.getMethod.getControlFlowGraph.getSuccsOf(u)) {
          seeds.addAll(generate(new Nothing(u, succ)))
        }
      }
    }
    AnalysisScope.LOGGER.info("Found {} seeds in {} in {} LOC .", seeds.size, watch, statementCount)
    seeds
  }

  protected def analyseClassInitializers = false

  protected def generate(seed: Nothing): Nothing
}