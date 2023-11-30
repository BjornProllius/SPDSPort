package boomerang.scene

import com.google.common.base.Stopwatch
import com.google.common.collect.{Lists, Sets}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

abstract class AnalysisScope(var cg: CallGraph) {

  private val LOGGER: Logger = LoggerFactory.getLogger(classOf[AnalysisScope])
  private var scanLibraryClasses: Boolean = false

  private val seeds: mutable.Set[Query] = Sets.newHashSet()

  private val processed: mutable.Set[Method] = Sets.newHashSet()
  private var statementCount: Int = 0

  def setScanLibraryClasses(enabled: Boolean): Unit = {
    scanLibraryClasses = enabled
  }

  def computeSeeds(): Collection[Query] = {
    val entryPoints: Collection[Method] = cg.getEntryPoints
    LOGGER.info("Computing seeds starting at {} entry method(s).", entryPoints.size)

    val watch: Stopwatch = Stopwatch.createStarted()
    val worklist: LinkedList[Method] = Lists.newLinkedList()
    worklist.addAll(entryPoints)
    while (!worklist.isEmpty) {
      val m: Method = worklist.pop()
      if (!processed.add(m)) {
        continue
      }
      LOGGER.trace("Processing {}", m)
      for (u <- m.getStatements) {
        statementCount += 1
        if (u.containsInvokeExpr()) {
          val edgesOutOf: Collection[CallGraph.Edge] = cg.edgesOutOf(u)
          for (e <- edgesOutOf) {
            val tgt: Method = e.tgt()
            if (!scanLibraryClasses && !tgt.getDeclaringClass.isApplicationClass) continue

            if (!processed.contains(tgt)) {
              worklist.add(tgt)
            }
          }
        }
        for (succ <- u.getMethod.getControlFlowGraph.getSuccsOf(u)) {
          seeds.addAll(generate(new ControlFlowGraph.Edge(u, succ)))
        }
      }
    }
    LOGGER.info("Found {} seeds in {} in {} LOC .", seeds.size, watch, statementCount)

    seeds
  }

  protected def analyseClassInitializers(): Boolean = {
    false
  }

  protected def generate(seed: ControlFlowGraph.Edge): Collection[_ <: Query]
}