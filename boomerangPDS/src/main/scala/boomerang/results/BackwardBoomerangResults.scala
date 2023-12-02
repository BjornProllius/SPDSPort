package boomerang.results

import boomerang.{BackwardQuery, ForwardQuery, Query, Util}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{Statement, Type, Val}
import boomerang.solver.{BackwardBoomerangSolver, ForwardBoomerangSolver}
import boomerang.stats.IBoomerangStats
import boomerang.util.{AccessPath, DefaultValueMap}
import com.google.common.base.Stopwatch
import com.google.common.collect.{Maps, Sets}
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}

import scala.collection.mutable.{Map, Set}

class BackwardBoomerangResults[W <: Weight] extends AbstractBoomerangResults[W]{

  class BackwardBoomerangResults[W <: Weight] (
    private val query: BackwardQuery,
    private val timedout: Boolean,
    private val queryToSolvers: DefaultValueMap[ForwardQuery, ForwardBoomerangSolver[W]],
    private val backwardSolver: BackwardBoomerangSolver[W],
    private val stats: IBoomerangStats[W],
    private var analysisWatch: Stopwatch
  ) extends AbstractBoomerangResults[W](queryToSolvers) {

    private var allocationSites: Map[ForwardQuery, Context] = _
    private var maxMemory: Long = Util.getReallyUsedMemory()

    stats.terminated(query, this)

    def getAllocationSites(): Map[ForwardQuery, Context] = {
      computeAllocations()
      allocationSites
    }

    def isTimedout(): Boolean = timedout

    def getStats(): IBoomerangStats[W] = stats

    def getAnalysisWatch(): Stopwatch = analysisWatch

    private def computeAllocations(): Unit = {
      if (allocationSites != null) return
      val results = Sets.newHashSet[ForwardQuery]()
      for ((fwKey, fwValue) <- queryToSolvers.entrySet()) {
        for (node <- fwValue.getFieldAutomaton().getInitialStates())
          fwValue.getFieldAutomaton().registerListener(
            new ExtractAllocationSiteStateListener[W](node, query, fwKey) {
              override def allocationSiteFound(allocationSite: ForwardQuery, query: BackwardQuery): Unit = {
                results.add(allocationSite)
              }
            }
          )
      }
      allocationSites = Maps.newHashMap[ForwardQuery, Context]()
      for (q <- results) {
        val context = constructContextGraph(q, query.asNode())
        assert(allocationSites.get(q) == null)
        allocationSites.put(q, context)
      }
    }
  }

  def aliases(el: Query): Boolean = {
    for (fw <- getAllocationSites().keySet) {
      if (queryToSolvers.getOrCreate(fw).getReachedStates().contains(el.asNode())) {
        if (queryToSolvers.getOrCreate(fw).reachesNodeWithEmptyField(el.asNode())) {
          return true
        }
      }
    }
    false
  }

  @deprecated
  def getAllAliases(stmt: Edge): Set[AccessPath] = {
    val results = Sets.newHashSet[AccessPath]()
    for (fw <- getAllocationSites().keySet) {
      queryToSolvers
        .getOrCreate(fw)
        .registerListener(
          new ExtractAllAliasListener[W](this.queryToSolvers.get(fw), results, stmt)
        )
    }
    results
  }

  @deprecated
  def getAllAliases(): Set[AccessPath] = {
    getAllAliases(query.cfgEdge())
  }

  def isEmpty(): Boolean = {
    computeAllocations()
    allocationSites.isEmpty
  }

  /**
   * Returns the set of types the backward analysis for the triggered query ever propagates.
   *
   * @return Set of types the backward analysis propagates
   */
  def getPropagationType(): Set[Type] = {
    val types = Sets.newHashSet[Type]()
    for (t <- backwardSolver.getCallAutomaton().getTransitions()) {
      if (!t.getStart().fact().isStatic()) types.add(t.getStart().fact().getType())
    }
    types
  }

  /**
   * Computes the set of statements (and variables at these statements) relevant for data-flow
   * propagation. A statement s is relevant, if a propagated variable x is used at s. I.e., when
   * propagting x @ y = x, the returned set contains x @ y = x, whereas it will not contain a call
   * site x @ y = foo(c), because x is not used at the statement.
   *
   * @return The set of relevant statements during data-flow propagation
   */
  @deprecated
  def getDataFlowPath(query: ForwardQuery): Set[Node[Edge, Val]] = {
    val dataFlowPath = Sets.newHashSet[Node[Edge, Val]]()
    val callAut = queryToSolvers.getOrCreate(query).getCallAutomaton()
    for (e <- callAut.getTransitionsToFinalWeights().entrySet()) {
      val t = e.getKey()
      if (t.getLabel().equals(Statement.epsilon())) return
      if (t.getStart().fact().isLocal() && !t.getLabel().getMethod().equals(t.getStart().fact().m())) return
      if (t.getLabel().getStart().uses(t.getStart().fact()))
        dataFlowPath.add(new Node[Edge, Val](t.getLabel(), t.getStart().fact()))
    }
    dataFlowPath
  }

  def getMaxMemory(): Long = {
    maxMemory
  }
}
