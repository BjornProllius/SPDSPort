package boomerang.debugger

import boomerang.ForwardQuery
import boomerang.callgraph.ObservableICFG
import boomerang.callgraph.ObservableStaticICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.scene.CallGraph
import boomerang.scene.CallGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.collect.HashMultimap
import com.google.common.collect.Multimap

import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.util.Collection
import java.util.HashSet
import java.util.Map
import java.util.Set

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import wpds.impl.Weight

/**
 * Can be used to obtain a dot file which can be plotted into a graphical representation of the call
 * graph. Call graph includes all edges and all methods which have edges incoming or outgoing.
 */
class CallGraphDebugger[W <: Weight] (dotFile: File, icfg: ObservableICFG[Statement, Method]) extends Debugger[W] {

  private val logger = LoggerFactory.getLogger(classOf[CallGraphDebugger[_]])
  private var callGraph: CallGraph = _

  private val totalCallSites = new HashSet[Statement]()
  private val virtualCallSites = HashMultimap.create[Statement, Method]()
  private var numVirtualCallSites: Int = _
  private var numVirtualCallSitesSingleTarget: Int = _
  private var numVirtualCallSitesMultipleTarget: Int = _
  private var avgNumTargetsVirtualCallSites: Float = _
  private var avgNumTargetMultiTargetCallSites: Float = _
  private val predecessors = HashMultimap.create[Method, Statement]()
  private var avgNumOfPredecessors: Float = _
  private var numOfEdgesInCallGraph: Int = _
  private var numEdgesFromPrecomputed: Int = _
  private var seedNumber: Int = 0

  override def done(
      icfg: ObservableICFG[Statement, Method],
      cfg: ObservableControlFlowGraph,
      visitedMethods: Set[Method],
      queryToSolvers: Map[ForwardQuery, ForwardBoomerangSolver[W]]): Unit = {

    if (icfg.isInstanceOf[ObservableStaticICFG]) {
      if (dotFile.exists()) {
        return
      }
    } else {
      seedNumber += 1
      if (seedNumber % 2 == 0) {
        return
      }
      val actualSeedNumber = seedNumber / 2 + 1
      var dotFileName = dotFile.getAbsolutePath()
      dotFileName = dotFileName.substring(0, dotFileName.lastIndexOf('.')) + actualSeedNumber + ".dot"
      dotFile = new File(dotFileName)
    }

    logger.info("Starting to compute visualization.")

    val stringBuilder = new StringBuilder()

    stringBuilder.append("digraph callgraph { \n")
    stringBuilder.append("node [margin=0, shape=box]; \n")

    addMethodsToDotfile(stringBuilder)

    stringBuilder.append("}")

    try {
      val file = new FileWriter(dotFile)
      logger.trace("Writing visualization to file {}", dotFile.getAbsolutePath())
      file.write(stringBuilder.toString())
      logger.info("Visualization available in file {}", dotFile.getAbsolutePath())
      file.close()
    } catch {
      case e: IOException =>
        logger.info(
          "Exception in writing to visualization file {} : {}",
          dotFile.getAbsolutePath(),
          e.getMessage())
    }
  }

  /**
   * Add all edges to string builder. The nodes between which edges run will be included, other
   * methods will not.
   */
  private def addMethodsToDotfile(stringBuilder: StringBuilder): Unit = {
    for (edge <- callGraph.getEdges()) {
      addMethodToDotFile(stringBuilder, edge.src().getMethod())
      stringBuilder.append(" -> ")
      addMethodToDotFile(stringBuilder, edge.tgt())
      stringBuilder.append("; \n")
    }
  }

  /**
   * Appends escaped method name to string builder, otherwise symbols like spaces mess with the dot
   * syntax
   */
  private def addMethodToDotFile(stringBuilder: StringBuilder, method: Method): Unit = {
    stringBuilder.append('"')
    stringBuilder.append(method)
    stringBuilder.append('"')
  }

  private def computeCallGraphStatistics(): Unit = {
    numOfEdgesInCallGraph = callGraph.size()
    for (edge <- callGraph.getEdges()) {
      val srcUnit = edge.src()
      totalCallSites.add(srcUnit)
      virtualCallSites.put(srcUnit, edge.tgt())
      predecessors.put(edge.tgt(), srcUnit)
    }
    computeVirtualCallSiteMetrics()
    computePredecessorMetrics()
    if (icfg != null) {
      numEdgesFromPrecomputed = icfg.getNumberOfEdgesTakenFromPrecomputedGraph()
    }
    if (numEdgesFromPrecomputed < 0) {
      numEdgesFromPrecomputed = numOfEdgesInCallGraph
    }
  }

  private def computeVirtualCallSiteMetrics(): Unit = {
    numVirtualCallSites = virtualCallSites.keySet().size()
    var totalTargetsVirtualCallSites = 0
    for (entry <- virtualCallSites.asMap().entrySet()) {
      val targets = entry.getValue().size()
      if (targets > 1) {
        numVirtualCallSitesMultipleTarget += 1
      } else if (targets == 1) {
        numVirtualCallSitesSingleTarget += 1
      }
      totalTargetsVirtualCallSites += targets
    }
    avgNumTargetsVirtualCallSites = totalTargetsVirtualCallSites / numVirtualCallSites.toFloat
    avgNumTargetMultiTargetCallSites = totalTargetsVirtualCallSites / numVirtualCallSitesMultipleTarget.toFloat
  }

  private def computePredecessorMetrics(): Unit = {
    val numMethods = predecessors.keySet().size()
    var totalPredecessors = 0
    for (entry <- predecessors.asMap().entrySet()) {
      totalPredecessors += entry.getValue().size()
    }
    avgNumOfPredecessors = totalPredecessors / numMethods.toFloat
  }

  def getCsvHeader(): String = {
    "numOfEdgesInCallGraph; totalCallSites; " +
      "virtualCallSites; virtualCallSitesSingleTarget; virtualCallSitesMultipleTarget; " +
      "avgNumTargetsVirtualCallSites; avgNumTargetMultiTargetCallSites;" +
      "avgNumOfPredecessors; edgesFromPrecomputed;"
  }

  def getCallGraphStatisticsAsCsv(): String = {
    computeCallGraphStatistics()
    numOfEdgesInCallGraph + ";" +
      totalCallSites.size + ";" +
      numVirtualCallSites + ";" +
      numVirtualCallSitesSingleTarget + ";" +
      numVirtualCallSitesMultipleTarget + ";" +
      avgNumTargetsVirtualCallSites + ";" +
      avgNumTargetMultiTargetCallSites + ";" +
      avgNumOfPredecessors + ";" +
      numEdgesFromPrecomputed + ";"
  }
}