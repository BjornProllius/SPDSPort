package boomerang

import boomerang.scene.{AnalysisScope, CallGraph, ControlFlowGraph, DataFlowScope, Statement}
import wpds.impl.Weight

import scala.collection.JavaConverters._

abstract class WholeProgramBoomerang[W <: Weight](cg: CallGraph, scope: DataFlowScope, opts: BoomerangOptions) 
  extends WeightedBoomerang[W](cg, scope, opts) {

  private var reachableMethodCount: Int = _
  private var allocationSites: Int = _
  private var callGraph: CallGraph = cg

  def this(cg: CallGraph, scope: DataFlowScope) = this(cg, scope, new DefaultBoomerangOptions())

  def wholeProgramAnalysis(): Unit = {
    val before = System.currentTimeMillis()
    val scope = new AnalysisScope(callGraph) {
      override protected def generate(cfgEdge: ControlFlowGraph.Edge): Collection[Query] = {
        val stmt = cfgEdge.start()
        if (stmt.isAssign && stmt.getRightOp.isNewExpr) {
          Collections.singleton(new ForwardQuery(cfgEdge, stmt.getRightOp))
        } else {
          Collections.emptySet[Query]
        }
      }
    }
    for (s <- scope.computeSeeds().asScala) {
      solve(s.asInstanceOf[ForwardQuery])
    }

    val after = System.currentTimeMillis()
    println(s"Analysis Time (in ms):\t${after - before}")
    println(s"Analyzed methods:\t$reachableMethodCount")
    println(s"Total solvers:\t${getSolvers.size()}")
    println(s"Allocation Sites:\t$allocationSites")
    println(options.statsFactory())
  }

  override protected def backwardSolve(query: BackwardQuery): Unit = {}
}