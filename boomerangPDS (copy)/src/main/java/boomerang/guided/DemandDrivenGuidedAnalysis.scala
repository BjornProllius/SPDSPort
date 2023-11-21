package boomerang.guided

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.BoomerangOptions
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.QueryGraph
import boomerang.guided.Specification.QueryDirection
import boomerang.results.AbstractBoomerangResults.Context
import boomerang.results.BackwardBoomerangResults
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Val
import boomerang.scene.jimple.SootCallGraph
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import com.google.common.collect.Table
import com.google.common.collect.Table.Cell
import java.util
import java.util.Map.Entry
import soot.Scene
import sync.pds.solver.nodes.Node
import wpds.impl.Weight.NoWeight

object DemandDrivenGuidedAnalysis {
  private class QueryWithContext {
    def this(query: Nothing) {
      this()
      this.query = query
    }

    def this(query: Nothing, triggeringNode: Nothing, parentQuery: Nothing) {
      this()
      this.query = query
      this.parentQuery = parentQuery
      this.triggeringNode = triggeringNode
    }

    private[guided] var query: Nothing = null
    private[guided] var parentQuery: Nothing = null
    private[guided] var triggeringNode: Nothing = null
  }
}

class DemandDrivenGuidedAnalysis(private val spec: Nothing, private val customBoomerangOptions: Nothing, private val scope: Nothing) {
  callGraph = new Nothing
  if (!customBoomerangOptions.allowMultipleQueries) throw new Nothing("Boomerang options allowMultipleQueries is set to false. Please enable it.")
  solver = new Nothing(callGraph, dataFlowScope, customBoomerangOptions)
  final private var callGraph: Nothing = null
  final private val queryQueue = Lists.newLinkedList
  final private val visited = Sets.newHashSet
  final private var solver: Nothing = null
  private var triggered = false

  def this(specification: Nothing, options: Nothing) {
    this(specification, options, SootDataFlowScope.make(Scene.v))
  }

  /**
   * The query graph takes as input an initial query from which all follow up computations are
   * computed. Based on the specification provided. It returns the QueryGraph which is a graph whose
   * nodes are Boomerang Queries, there is an edge between the queries if there node A triggered a
   * subsequent query B.
   *
   * <p>Important note: Ensure to call cleanUp() after finishing the analysis.
   *
   * @param query The initial query to start the analysis from.
   * @return a query graph containing all queries triggered.
   */
  def run(query: Nothing): Nothing = {
    if (triggered) throw new Nothing(classOf[DemandDrivenGuidedAnalysis].getName + " must be instantiated once per query. Use a different instance.")
    triggered = true
    queryQueue.add(new DemandDrivenGuidedAnalysis.QueryWithContext(query))
    while (!queryQueue.isEmpty) {
      val pop = queryQueue.pop
      if (pop.query.isInstanceOf[Nothing]) {
        var results: Nothing = null
        val currentQuery = pop.query.asInstanceOf[Nothing]
        if (pop.parentQuery == null) results = solver.solve(currentQuery)
        else results = solver.solveUnderScope(currentQuery, pop.triggeringNode, pop.parentQuery)
        val forwardResults = results.asStatementValWeightTable(pop.query.asInstanceOf[Nothing])
        // Any ForwardQuery may trigger additional ForwardQuery under its own scope.
        triggerNewBackwardQueries(forwardResults, currentQuery, QueryDirection.FORWARD)
      }
      else {
        var results: Nothing = null
        if (pop.parentQuery == null) results = solver.solve(pop.query.asInstanceOf[Nothing])
        else results = solver.solveUnderScope(pop.query.asInstanceOf[Nothing], pop.triggeringNode, pop.parentQuery)
        val backwardResults = solver.getBackwardSolvers.get(query).asStatementValWeightTable
        triggerNewBackwardQueries(backwardResults, pop.query, QueryDirection.BACKWARD)
        val allocationSites = results.getAllocationSites
        import scala.collection.JavaConversions._
        for (entry <- allocationSites.entrySet) {
          triggerNewBackwardQueries(results.asStatementValWeightTable(entry.getKey), entry.getKey, QueryDirection.FORWARD)
        }
      }
    }
    val queryGraph = solver.getQueryGraph
    queryGraph
  }

  /**
   * Ensure to call cleanup to detach all listeners from the solver, otherwise the analysis may run
   * into a Memory issues.
   */
  def cleanUp(): Unit = {
    solver.unregisterAllListeners
  }

  def getSolver: Nothing = solver

  private def triggerNewBackwardQueries(backwardResults: Nothing, lastQuery: Nothing, direction: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (cell <- backwardResults.cellSet) {
      val triggeringEdge = cell.getRowKey
      val fact = cell.getColumnKey
      var queries: Nothing = null
      if (direction eq QueryDirection.FORWARD) queries = spec.onForwardFlow(lastQuery.asInstanceOf[Nothing], cell.getRowKey, cell.getColumnKey)
      else queries = spec.onBackwardFlow(lastQuery.asInstanceOf[Nothing], cell.getRowKey, cell.getColumnKey)
      import scala.collection.JavaConversions._
      for (q <- queries) {
        addToQueue(new DemandDrivenGuidedAnalysis.QueryWithContext(q, new Nothing(triggeringEdge, fact), lastQuery))
      }
    }
  }

  private def addToQueue(nextQuery: DemandDrivenGuidedAnalysis.QueryWithContext): Unit = {
    if (visited.add(nextQuery.query)) queryQueue.add(nextQuery)
  }
}