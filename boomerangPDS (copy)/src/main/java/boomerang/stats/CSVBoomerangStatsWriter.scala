/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang.stats

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.Util
import boomerang.WeightedBoomerang
import boomerang.results.BackwardBoomerangResults
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Field.ArrayField
import boomerang.scene.Method
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.base.Joiner
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.nio.file.Files
import java.util
import java.util.Comparator
import java.util.concurrent.TimeUnit
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Rule
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.interfaces.Location
import wpds.interfaces.State

object CSVBoomerangStatsWriter {
  private val CSV_SEPARATOR = ";"

  private object Headers extends Enumeration {
    type Headers = Value
    val Query, QueryType, FieldTransitions, CallTransitions, CallRules, FieldRules, ReachedForwardNodes, ReachedBackwardNodes, CallVisitedMethods, FieldVisitedMethods, CallVisitedStmts, FieldVisitedStmts, FieldWritePOIs, FieldReadPOIs, StaticFlows, ArrayFlows, QueryTime, Timeout, ICFGEdges, CallGeneratedStates, FieldGeneratedStates, FieldLongestAccessPath, CallLongestCallStack, CallContainsLoop, FieldContainsLoop, MemoryBefore, MemoryAfter, MemoryDiff = Value
  }

  def sortByValues[K](map: Nothing): Nothing = {
    val valueComparator = new Nothing() {
      def compare(k1: K, k2: K): Int = if (map.get(k2) > map.get(k1)) 1
      else -1
    }
    val sortedByValues = new Nothing(valueComparator)
    sortedByValues.putAll(map)
    sortedByValues
  }

  private class WeightedTransition[X <: Location, Y <: State, W](private[stats] val t: Nothing, private[stats] val w: W) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (t == null) 0
      else t.hashCode)
      result = prime * result + (if (w == null) 0
      else w.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[CSVBoomerangStatsWriter.WeightedTransition[_ <: Nothing, _ <: Nothing, _]]
      if (t == null) if (other.t != null) return false
      else if (!t.equals(other.t)) return false
      if (w == null) if (other.w != null) return false
      else if (!w.equals(other.w)) return false
      true
    }
  }
}

class CSVBoomerangStatsWriter[W <: Weight](private var outputFileName: Nothing) extends Nothing {
  for (h <- CSVBoomerangStatsWriter.Headers.values) {
    this.headers.add(h.toString)
  }
  memoryBefore = Util.getReallyUsedMemory
  private val queries = Maps.newHashMap
  private val globalFieldTransitions = Sets.newHashSet
  private var fieldTransitionCollisions = 0
  private val globalCallTransitions = Sets.newHashSet
  private var callTransitionCollisions = 0
  private val globalFieldRules = Sets.newHashSet
  private var fieldRulesCollisions = 0
  private val globalCallRules = Sets.newHashSet
  private var callRulesCollisions = 0
  private val reachedForwardNodes = Sets.newHashSet
  private var reachedForwardNodeCollisions = 0
  private val reachedBackwardNodes = Sets.newHashSet
  private var reachedBackwardNodeCollisions = 0
  private val callVisitedMethods = Sets.newHashSet
  private val fieldVisitedMethods = Sets.newHashSet
  private val callVisitedStmts = Sets.newHashSet
  private val fieldVisitedStmts = Sets.newHashSet
  private val fieldGeneratedStates = Sets.newHashSet
  private val callGeneratedStates = Sets.newHashSet
  private var arrayFlows = 0
  private var staticFlows = 0
  private var fieldWritePOIs = 0
  private val fieldReadPOIs = 0
  private val headers = Lists.newArrayList
  private val headersToValues = Maps.newHashMap
  private var memoryBefore = 0L

  @Override def registerSolver(key: Nothing, solver: Nothing): Unit = {
    if (queries.containsKey(key)) return
    queries.put(key, solver)
    solver.getFieldAutomaton.registerListener((t, w, aut) => {
      if (!globalFieldTransitions.add(new CSVBoomerangStatsWriter.WeightedTransition[Nothing, Nothing, W](t, w))) fieldTransitionCollisions += 1
      fieldVisitedMethods.add(t.getStart.fact.stmt.getMethod)
      fieldVisitedStmts.add(t.getStart.fact.stmt)
      if (t.getLabel.isInstanceOf[Nothing]) arrayFlows += 1
      addFieldGeneratedState(t.getStart)
      addFieldGeneratedState(t.getTarget)
    })
    solver.getCallAutomaton.registerListener((t, w, aut) => {
      if (!globalCallTransitions.add(new CSVBoomerangStatsWriter.WeightedTransition[X, Y, W](t, w))) callTransitionCollisions += 1
      callVisitedMethods.add(t.getLabel.getMethod)
      fieldVisitedStmts.add(t.getLabel)
      if (t.getStart.fact.isStatic) staticFlows += 1
      addCallGeneratedState(t.getStart)
      addCallGeneratedState(t.getTarget)
    })
    solver.getFieldPDS.registerUpdateListener((rule) => {
      if (!globalFieldRules.add(rule)) fieldRulesCollisions += 1
    })
    solver.getCallPDS.registerUpdateListener((rule) => {
      if (!globalCallRules.add(rule)) callRulesCollisions += 1
    })
    solver.registerListener((reachableNode) => {
      if (solver.isInstanceOf[Nothing]) if (!reachedForwardNodes.add(reachableNode)) reachedForwardNodeCollisions += 1
      else if (!reachedBackwardNodes.add(reachableNode)) reachedBackwardNodeCollisions += 1
    })
  }

  protected def addFieldGeneratedState(s: Nothing): Unit = {
    if (s.isInstanceOf[Nothing]) fieldGeneratedStates.add(s)
  }

  protected def addCallGeneratedState(s: Nothing): Unit = {
    if (s.isInstanceOf[Nothing]) callGeneratedStates.add(s)
  }

  @Override def registerFieldWritePOI(key: Nothing): Unit = {
    fieldWritePOIs += 1
  }

  def toString: Nothing = {
    var s = "=========== Boomerang Stats =============\n"
    var forwardQuery = 0
    var backwardQuery = 0
    import scala.collection.JavaConversions._
    for (q <- queries.keySet) {
      if (q.isInstanceOf[Nothing]) forwardQuery += 1
      else backwardQuery += 1
    }
    s += String.format("Queries (Forward/Backward/Total): \t\t %s/%s/%s\n", forwardQuery, backwardQuery, queries.keySet.size)
    s += String.format("Visited Methods (Field/Call): \t\t %s/%s\n", fieldVisitedMethods.size, callVisitedMethods.size)
    s += String.format("Reached Forward Nodes(Collisions): \t\t %s (%s)\n", reachedForwardNodes.size, reachedForwardNodeCollisions)
    s += String.format("Reached Backward Nodes(Collisions): \t\t %s (%s)\n", reachedBackwardNodes.size, reachedBackwardNodeCollisions)
    s += String.format("Global Field Rules(Collisions): \t\t %s (%s)\n", globalFieldRules.size, fieldRulesCollisions)
    s += String.format("Global Field Transitions(Collisions): \t\t %s (%s)\n", globalFieldTransitions.size, fieldTransitionCollisions)
    s += String.format("Global Call Rules(Collisions): \t\t %s (%s)\n", globalCallRules.size, callRulesCollisions)
    s += String.format("Global Call Transitions(Collisions): \t\t %s (%s)\n", globalCallTransitions.size, callTransitionCollisions)
    s += String.format("Special Flows (Static/Array): \t\t %s(%s)/%s(%s)\n", staticFlows, globalCallTransitions.size, arrayFlows, globalFieldTransitions.size)
    s += computeMetrics
    s += "\n"
    s
  }

  @Override def getCallVisitedMethods: Nothing = Sets.newHashSet(callVisitedMethods)

  private def computeMetrics = {
    var min = Integer.MAX_VALUE
    var totalReached = 0
    var max = 0
    var maxQuery: Nothing = null
    import scala.collection.JavaConversions._
    for (q <- queries.keySet) {
      val size = queries.get(q).getReachedStates.size
      totalReached += size
      min = Math.min(size, min)
      if (size > max) maxQuery = q
      max = Math.max(size, max)
    }
    val average = totalReached.toFloat / queries.keySet.size
    var s = String.format("Reachable nodes (Min/Avg/Max): \t\t%s/%s/%s\n", min, average, max)
    s += String.format("Maximal Query: \t\t%s\n", maxQuery)
    s
  }

  @Override def getForwardReachesNodes: Nothing = {
    val res = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (q <- queries.keySet) {
      if (q.isInstanceOf[Nothing]) res.addAll(queries.get(q).getReachedStates)
    }
    res
  }

  @Override def terminated(query: Nothing, res: Nothing): Unit = {
    writeToFile(query, res.getAnalysisWatch.elapsed(TimeUnit.MILLISECONDS), res.isTimedout)
  }

  @Override def terminated(query: Nothing, res: Nothing): Unit = {
    writeToFile(query, res.getAnalysisWatch.elapsed(TimeUnit.MILLISECONDS), res.isTimedout)
  }

  private def writeToFile(query: Nothing, queryTime: Long, timeout: Boolean): Unit = {
    val memoryAfter = Util.getReallyUsedMemory
    put(CSVBoomerangStatsWriter.Headers.Query, query.toString)
    put(CSVBoomerangStatsWriter.Headers.QueryType, if (query.isInstanceOf[Nothing]) "B"
    else "F")
    put(CSVBoomerangStatsWriter.Headers.QueryTime, queryTime)
    put(CSVBoomerangStatsWriter.Headers.Timeout, if (timeout) "1"
    else "0")
    put(CSVBoomerangStatsWriter.Headers.ArrayFlows, arrayFlows)
    put(CSVBoomerangStatsWriter.Headers.CallRules, globalCallRules.size)
    put(CSVBoomerangStatsWriter.Headers.FieldRules, globalFieldRules.size)
    put(CSVBoomerangStatsWriter.Headers.CallTransitions, globalCallTransitions.size)
    put(CSVBoomerangStatsWriter.Headers.FieldTransitions, globalFieldTransitions.size)
    put(CSVBoomerangStatsWriter.Headers.FieldReadPOIs, fieldReadPOIs)
    put(CSVBoomerangStatsWriter.Headers.FieldWritePOIs, fieldWritePOIs)
    put(CSVBoomerangStatsWriter.Headers.FieldVisitedMethods, fieldVisitedMethods.size)
    put(CSVBoomerangStatsWriter.Headers.CallVisitedMethods, callVisitedMethods.size)
    put(CSVBoomerangStatsWriter.Headers.FieldVisitedStmts, fieldVisitedStmts.size)
    put(CSVBoomerangStatsWriter.Headers.CallVisitedStmts, callVisitedStmts.size)
    put(CSVBoomerangStatsWriter.Headers.ReachedForwardNodes, reachedForwardNodes.size)
    put(CSVBoomerangStatsWriter.Headers.ReachedBackwardNodes, reachedBackwardNodes.size)
    put(CSVBoomerangStatsWriter.Headers.StaticFlows, staticFlows)
    // TODO implement
    //        put(Headers.ICFGEdges, Util.getICFGEdges());
    put(CSVBoomerangStatsWriter.Headers.CallGeneratedStates, callGeneratedStates.size)
    put(CSVBoomerangStatsWriter.Headers.FieldGeneratedStates, fieldGeneratedStates.size)
    put(CSVBoomerangStatsWriter.Headers.CallLongestCallStack, queries.get(query).getCallAutomaton.getLongestPath.size)
    put(CSVBoomerangStatsWriter.Headers.FieldLongestAccessPath, queries.get(query).getFieldAutomaton.getLongestPath.size)
    put(CSVBoomerangStatsWriter.Headers.CallContainsLoop, queries.get(query).getCallAutomaton.containsLoop)
    put(CSVBoomerangStatsWriter.Headers.FieldContainsLoop, queries.get(query).getFieldAutomaton.containsLoop)
    put(CSVBoomerangStatsWriter.Headers.MemoryAfter, memoryAfter)
    put(CSVBoomerangStatsWriter.Headers.MemoryBefore, memoryBefore)
    put(CSVBoomerangStatsWriter.Headers.MemoryDiff, memoryAfter - memoryBefore)
    try {
      val reportFile = new Nothing(outputFileName).getAbsoluteFile
      if (!reportFile.getParentFile.exists) try Files.createDirectories(reportFile.getParentFile.toPath)
      catch {
        case e: Nothing =>
          throw new Nothing("Was not able to create directories for IDEViz output!")
      }
      val fileExisted = reportFile.exists
      val writer = new Nothing(reportFile, true)
      if (!fileExisted) writer.write(Joiner.on(CSVBoomerangStatsWriter.CSV_SEPARATOR).join(headers) + "\n")
      val line = Lists.newArrayList
      import scala.collection.JavaConversions._
      for (h <- headers) {
        var string = headersToValues.get(h)
        if (string == null) string = ""
        line.add(string)
      }
      writer.write(Joiner.on(CSVBoomerangStatsWriter.CSV_SEPARATOR).join(line) + "\n")
      writer.close
    } catch {
      case e: Nothing =>
        e.printStackTrace
    }
  }

  private def put(key: Nothing, `val`: Nothing): Unit = {
    if (!headers.contains(key)) System.err.println("Did not create a header to this value " + key)
    else headersToValues.put(key, `val`.toString)
  }

  private def put(key: CSVBoomerangStatsWriter.Headers, `val`: Nothing): Unit = {
    put(key.toString, `val`)
  }
}