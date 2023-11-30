package boomerang.guided

import boomerang.{BackwardQuery, Boomerang, BoomerangOptions, ForwardQuery, Query, QueryGraph}
import boomerang.guided.Specification.QueryDirection
import boomerang.results.AbstractBoomerangResults.Context
import boomerang.results.{BackwardBoomerangResults, ForwardBoomerangResults}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{DataFlowScope, SootDataFlowScope, Val}
import boomerang.scene.jimple.SootCallGraph
import com.google.common.collect.{Lists, Sets, Table, Table.Cell}
import java.util.{Collection, LinkedList, Map, Set}
import soot.Scene
import sync.pds.solver.nodes.Node
import wpds.impl.Weight.NoWeight

class DemandDrivenGuidedAnalysis(specification: IDemandDrivenGuidedManager, options: BoomerangOptions, dataFlowScope: DataFlowScope) {
  private val customBoomerangOptions: BoomerangOptions = options
  private val spec: IDemandDrivenGuidedManager = specification
  private val scope: DataFlowScope = dataFlowScope
  private val callGraph: SootCallGraph = new SootCallGraph()
  private val queryQueue: LinkedList[QueryWithContext] = Lists.newLinkedList()
  private val visited: Set[Query] = Sets.newHashSet()
  private val solver: Boomerang = new Boomerang(callGraph, scope, customBoomerangOptions)
  private var triggered: Boolean = false

  if (!options.allowMultipleQueries()) {
    throw new RuntimeException("Boomerang options allowMultipleQueries is set to false. Please enable it.")
  }
}

object DemandDrivenGuidedAnalysis {
  def apply(specification: IDemandDrivenGuidedManager, options: BoomerangOptions): DemandDrivenGuidedAnalysis = {
    new DemandDrivenGuidedAnalysis(specification, options, SootDataFlowScope.make(Scene.v()))
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
  def run(query: Query): QueryGraph[NoWeight] = {
    if (triggered) {
      throw new RuntimeException(
        DemandDrivenGuidedAnalysis.getClass.getName
          + " must be instantiated once per query. Use a different instance.")
    }
    triggered = true
    queryQueue.add(new QueryWithContext(query))
    while (!queryQueue.isEmpty) {
      val pop = queryQueue.pop()
      if (pop.query.isInstanceOf[ForwardQuery]) {
        var results: ForwardBoomerangResults[NoWeight] = null
        val currentQuery = pop.query.asInstanceOf[ForwardQuery]
        if (pop.parentQuery == null) {
          results = solver.solve(currentQuery)
        } else {
          results = solver.solveUnderScope(currentQuery, pop.triggeringNode, pop.parentQuery)
        }

        val forwardResults = results.asStatementValWeightTable(pop.query.asInstanceOf[ForwardQuery])
        // Any ForwardQuery may trigger additional ForwardQuery under its own scope.
        triggerNewBackwardQueries(forwardResults, currentQuery, QueryDirection.FORWARD)
      } else {
        var results: BackwardBoomerangResults[NoWeight] = null
        if (pop.parentQuery == null) {
          results = solver.solve(pop.query.asInstanceOf[BackwardQuery])
        } else {
          results = solver.solveUnderScope(pop.query.asInstanceOf[BackwardQuery], pop.triggeringNode, pop.parentQuery)
        }
        val backwardResults = solver.getBackwardSolvers.get(query).asStatementValWeightTable()

        triggerNewBackwardQueries(backwardResults, pop.query, QueryDirection.BACKWARD)
        val allocationSites = results.getAllocationSites

        for (entry <- allocationSites.entrySet().asScala) {
          triggerNewBackwardQueries(
            results.asStatementValWeightTable(entry.getKey),
            entry.getKey,
            QueryDirection.FORWARD)
        }
      }
    }

    val queryGraph = solver.getQueryGraph
    queryGraph
  }
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import com.google.common.collect.Table
import wpds.impl.Weight.NoWeight
import wpds.impl.nodes.Node

import scala.collection.JavaConverters._

/**
 * Ensure to call cleanup to detach all listeners from the solver, otherwise the analysis may run
 * into a Memory issues.
 */
def cleanUp(): Unit = {
  solver.unregisterAllListeners()
}

def getSolver: Boomerang = {
  solver
}

private def triggerNewBackwardQueries(
    backwardResults: Table[Edge, Val, NoWeight], 
    lastQuery: Query, 
    direction: QueryDirection): Unit = {
  for (cell <- backwardResults.cellSet().asScala) {
    val triggeringEdge = cell.getRowKey
    val fact = cell.getColumnKey
    val queries = direction match {
      case QueryDirection.FORWARD =>
        spec.onForwardFlow(lastQuery.asInstanceOf[ForwardQuery], cell.getRowKey, cell.getColumnKey)
      case _ =>
        spec.onBackwardFlow(lastQuery.asInstanceOf[BackwardQuery], cell.getRowKey, cell.getColumnKey)
    }
    for (q <- queries.asScala) {
      addToQueue(new QueryWithContext(q, new Node(triggeringEdge, fact), lastQuery))
    }
  }
}

private def addToQueue(nextQuery: QueryWithContext): Unit = {
  if (visited.add(nextQuery.query)) {
    queryQueue.add(nextQuery)
  }
}
// this bit is wierd and i don't trust it's translation from java
private class QueryWithContext(var query: Query, var triggeringNode: Node[Edge, Val] = null, var parentQuery: Query = null)
}
