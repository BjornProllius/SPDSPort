package boomerang.results

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.Util
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import boomerang.scene.Type
import boomerang.scene.Val
import boomerang.solver.BackwardBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import boomerang.stats.IBoomerangStats
import boomerang.util.AccessPath
import boomerang.util.DefaultValueMap
import com.google.common.base.Stopwatch
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import java.util
import java.util.Map.Entry
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton

class BackwardBoomerangResults[W <: Weight](private val query: Nothing, private val timedout: Boolean, queryToSolvers: Nothing, private val backwardSolver: Nothing, private val stats: Nothing, private var analysisWatch: Nothing) extends Nothing(queryToSolvers) {
  stats.terminated(query, this)
  maxMemory = Util.getReallyUsedMemory
  private var allocationSites: Nothing = null
  private var maxMemory = 0L

  def getAllocationSites: Nothing = {
    computeAllocations()
    allocationSites
  }

  def isTimedout: Boolean = timedout

  def getStats: Nothing = stats

  def getAnalysisWatch: Nothing = analysisWatch

  private def computeAllocations(): Unit = {
    if (allocationSites != null) return
    val results = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (fw <- queryToSolvers.entrySet) {
      import scala.collection.JavaConversions._
      for (node <- fw.getValue.getFieldAutomaton.getInitialStates) {
        fw.getValue.getFieldAutomaton.registerListener(new Nothing((node, query, fw.getKey.asInstanceOf[Nothing])) {
          @Override protected def allocationSiteFound(allocationSite: Nothing, query: Nothing): Unit = {
            results.add(allocationSite)
          }
        })
      }
    }
    allocationSites = Maps.newHashMap
    import scala.collection.JavaConversions._
    for (q <- results) {
      val context = constructContextGraph(q, query.asNode)
      assert(allocationSites.get(q) == null)
      allocationSites.put(q, context)
    }
  }

  def aliases(el: Nothing): Boolean = {
    import scala.collection.JavaConversions._
    for (fw <- getAllocationSites.keySet) {
      if (queryToSolvers.getOrCreate(fw).getReachedStates.contains(el.asNode)) if (queryToSolvers.getOrCreate(fw).reachesNodeWithEmptyField(el.asNode)) return true
    }
    false
  }

  @deprecated def getAllAliases(stmt: Nothing): Nothing = {
    val results = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (fw <- getAllocationSites.keySet) {
      queryToSolvers.getOrCreate(fw).registerListener(new Nothing(this.queryToSolvers.get(fw), results, stmt))
    }
    results
  }

  @deprecated def getAllAliases: Nothing = getAllAliases(query.cfgEdge)

  def isEmpty: Boolean = {
    computeAllocations()
    allocationSites.isEmpty
  }

  /**
   * Returns the set of types the backward analysis for the triggered query ever propagates.
   *
   * @return Set of types the backward analysis propagates
   */
  def getPropagationType: Nothing = {
    val types = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (t <- backwardSolver.getCallAutomaton.getTransitions) {
      if (!t.getStart.fact.isStatic) types.add(t.getStart.fact.getType)
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
  @deprecated def getDataFlowPath(query: Nothing): Nothing = {
    val dataFlowPath = Sets.newHashSet
    val callAut = queryToSolvers.getOrCreate(query).getCallAutomaton
    import scala.collection.JavaConversions._
    for (e <- callAut.getTransitionsToFinalWeights.entrySet) {
      val t = e.getKey
      if (t.getLabel.equals(Statement.epsilon)) continue //todo: continue is not supported
      if (t.getStart.fact.isLocal && !t.getLabel.getMethod.equals(t.getStart.fact.m)) continue //todo: continue is not supported
      if (t.getLabel.getStart.uses(t.getStart.fact)) dataFlowPath.add(new Nothing(t.getLabel, t.getStart.fact))
    }
    dataFlowPath
  }

  def getMaxMemory: Long = maxMemory
}