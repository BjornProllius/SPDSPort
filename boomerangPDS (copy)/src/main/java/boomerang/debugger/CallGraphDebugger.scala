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
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import wpds.impl.Weight

/**
 * Can be used to obtain a dot file which can be plotted into a graphical representation of the call
 * graph. Call graph includes all edges and all methods which have edges incoming or outgoing.
 */
object CallGraphDebugger {
  private val logger = LoggerFactory.getLogger(classOf[CallGraphDebugger[_ <: Nothing]])
  private var seedNumber = 0
}

class CallGraphDebugger[W <: Weight](private var dotFile: Nothing, private var icfg: Nothing) extends Nothing {
  private val callGraph: Nothing = null
  private val totalCallSites = new Nothing
  private val virtualCallSites = HashMultimap.create
  private var numVirtualCallSites = 0
  private var numVirtualCallSitesSingleTarget = 0
  private var numVirtualCallSitesMultipleTarget = 0
  private var avgNumTargetsVirtualCallSites = .0
  private var avgNumTargetMultiTargetCallSites = .0
  private val predecessors = HashMultimap.create
  private var avgNumOfPredecessors = .0
  private var numOfEdgesInCallGraph = 0
  private var numEdgesFromPrecomputed = 0

  @Override def done(icfg: Nothing, cfg: Nothing, visitedMethods: Nothing, queryToSolvers: Nothing): Unit = {
    //        callGraph = icfg.getCallGraphCopy();
    // Check if we have a static icfg, so the call graph does not change across seeds
    if (icfg.isInstanceOf[Nothing]) {
      // Check if we already have made a dot file for that icfg once
      if (dotFile.exists) {
        // Then we do not need to do it again
        return
      }
    }
    else {
      // We have a dynamic icfg that is different for every seed. Enumerate the files
      CallGraphDebugger.seedNumber += 1
      // The call graph debugger becomes active for both phases of the IDEALSeedSolver, and they
      // operate on the
      // same call graph so do not output that
      // call graph twice
      if (CallGraphDebugger.seedNumber % 2 == 0) return
      val actualSeedNumber = CallGraphDebugger.seedNumber / 2 + 1
      var dotFileName = dotFile.getAbsolutePath
      dotFileName = dotFileName.substring(0, dotFileName.lastIndexOf('.')) + actualSeedNumber + ".dot"
      dotFile = new Nothing(dotFileName)
    }
    CallGraphDebugger.logger.info("Starting to compute visualization.")
    // Use string builder to get text for call graph
    val stringBuilder = new Nothing
    // Needed to make graph in dot
    stringBuilder.append("digraph callgraph { \n")
    stringBuilder.append("node [margin=0, shape=box]; \n")
    // Add content of graph
    addMethodsToDotfile(stringBuilder)
    // End graph
    stringBuilder.append("}")
    // Write out what was gathered in the string builder
    try {
      val file = new Nothing(dotFile)
      try {
        CallGraphDebugger.logger.trace("Writing visualization to file {}", dotFile.getAbsolutePath)
        file.write(stringBuilder.toString)
        CallGraphDebugger.logger.info("Visualization available in file {}", dotFile.getAbsolutePath)
      } catch {
        case e: Nothing =>
          CallGraphDebugger.logger.info("Exception in writing to visualization file {} : {}", dotFile.getAbsolutePath, e.getMessage)
      } finally if (file != null) file.close()
    }
  }

  /**
   * Add all edges to string builder. The nodes between which edges run will be included, other
   * methods will not.
   */
  private def addMethodsToDotfile(stringBuilder: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (edge <- callGraph.getEdges) {
      addMethodToDotFile(stringBuilder, edge.src.getMethod)
      stringBuilder.append(" -> ")
      addMethodToDotFile(stringBuilder, edge.tgt)
      stringBuilder.append("; \n")
    }
  }

  /**
   * Appends escaped method name to string builder, otherwise symbols like spaces mess with the dot
   * syntax
   */
  private def addMethodToDotFile(stringBuilder: Nothing, method: Nothing): Unit = {
    stringBuilder.append('"')
    stringBuilder.append(method)
    stringBuilder.append('"')
  }

  private def computeCallGraphStatistics(): Unit = {
    numOfEdgesInCallGraph = callGraph.size
    import scala.collection.JavaConversions._
    for (edge <- callGraph.getEdges) {
      val srcUnit = edge.src
      totalCallSites.add(srcUnit)
      //            if (edge.kind().equals(Kind.VIRTUAL)) {
      virtualCallSites.put(srcUnit, edge.tgt)
      predecessors.put(edge.tgt, srcUnit)
    }
    computeVirtualCallSiteMetrics()
    computePredecessorMetrics()
    if (icfg != null) numEdgesFromPrecomputed = icfg.getNumberOfEdgesTakenFromPrecomputedGraph
    if (numEdgesFromPrecomputed < 0) numEdgesFromPrecomputed = numOfEdgesInCallGraph
  }

  private def computeVirtualCallSiteMetrics(): Unit = {
    numVirtualCallSites = virtualCallSites.keySet.size
    var totalTargetsVirtualCallSites = 0
    import scala.collection.JavaConversions._
    for (entry <- virtualCallSites.asMap.entrySet) {
      val targets = entry.getValue.size
      if (targets > 1) numVirtualCallSitesMultipleTarget += 1
      else if (targets == 1) numVirtualCallSitesSingleTarget += 1
      totalTargetsVirtualCallSites += targets
    }
    avgNumTargetsVirtualCallSites = totalTargetsVirtualCallSites / numVirtualCallSites.toFloat
    avgNumTargetMultiTargetCallSites = totalTargetsVirtualCallSites / numVirtualCallSitesMultipleTarget.toFloat
  }

  private def computePredecessorMetrics(): Unit = {
    val numMethods = predecessors.keySet.size
    var totalPredecessors = 0
    import scala.collection.JavaConversions._
    for (entry <- predecessors.asMap.entrySet) {
      totalPredecessors += entry.getValue.size
    }
    avgNumOfPredecessors = totalPredecessors / numMethods.toFloat
  }

  def getCsvHeader: Nothing = "numOfEdgesInCallGraph; totalCallSites; " + "virtualCallSites; virtualCallSitesSingleTarget; virtualCallSitesMultipleTarget; " + "avgNumTargetsVirtualCallSites; avgNumTargetMultiTargetCallSites;" + "avgNumOfPredecessors; edgesFromPrecomputed;"

  def getCallGraphStatisticsAsCsv: Nothing = {
    computeCallGraphStatistics()
    String.valueOf(numOfEdgesInCallGraph) + ';' + totalCallSites.size + ';' + numVirtualCallSites + ';' + numVirtualCallSitesSingleTarget + ';' + numVirtualCallSitesMultipleTarget + ';' + avgNumTargetsVirtualCallSites + ';' + avgNumTargetMultiTargetCallSites + ';' + avgNumOfPredecessors + ';' + numEdgesFromPrecomputed + ';'
  }
}