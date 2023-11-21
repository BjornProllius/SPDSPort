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
import boomerang.WeightedBoomerang
import boomerang.results.BackwardBoomerangResults
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Field.ArrayField
import boomerang.scene.Method
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.BackwardBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import java.util
import java.util.Comparator
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Rule
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.interfaces.Location
import wpds.interfaces.State

object AdvancedBoomerangStats {
  def sortByValues[K](map: Nothing): Nothing = {
    val valueComparator = (k1, k2) => {
      if (map.get(k2) > map.get(k1)) 1
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
      val other = obj.asInstanceOf[AdvancedBoomerangStats.WeightedTransition[_ <: Nothing, _ <: Nothing, _]]
      if (t == null) if (other.t != null) return false
      else if (!t.equals(other.t)) return false
      if (w == null) if (other.w != null) return false
      else if (!w.equals(other.w)) return false
      true
    }
  }
}

class AdvancedBoomerangStats[W <: Weight] extends Nothing {
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
  private var arrayFlows = 0
  private var staticFlows = 0
  private val COUNT_TOP_METHODS = false
  private val backwardFieldMethodsRules = new Nothing
  private val backwardCallMethodsRules = new Nothing
  private val forwardFieldMethodsRules = new Nothing
  private val forwardCallMethodsRules = new Nothing

  @Override def registerSolver(key: Nothing, solver: Nothing): Unit = {
    if (queries.containsKey(key)) return
    queries.put(key, solver)
    solver.getFieldAutomaton.registerListener((t, w, aut) => {
      if (!globalFieldTransitions.add(new AdvancedBoomerangStats.WeightedTransition[X, Y, W](t, w))) fieldTransitionCollisions += 1
      fieldVisitedMethods.add(t.getStart.fact.stmt.getMethod)
      if (t.getLabel.isInstanceOf[Nothing]) arrayFlows += 1
    })
    solver.getCallAutomaton.registerListener((t, w, aut) => {
      if (!globalCallTransitions.add(new AdvancedBoomerangStats.WeightedTransition[X, Y, W](t, w))) callTransitionCollisions += 1
      callVisitedMethods.add(t.getLabel.getMethod)
      if (t.getStart.fact.isStatic) staticFlows += 1
    })
    solver.getFieldPDS.registerUpdateListener((rule) => {
      if (!globalFieldRules.add(rule)) fieldRulesCollisions += 1
      else if (COUNT_TOP_METHODS) increaseMethod(rule.getS1.fact.stmt.getMethod.toString, if (solver.isInstanceOf[Nothing]) backwardFieldMethodsRules
      else forwardFieldMethodsRules)
    })
    solver.getCallPDS.registerUpdateListener((rule) => {
      if (!globalCallRules.add(rule)) callRulesCollisions += 1
      else if (COUNT_TOP_METHODS) increaseMethod(rule.getL1.getMethod.toString, if (solver.isInstanceOf[Nothing]) backwardCallMethodsRules
      else forwardCallMethodsRules)
    })
    solver.registerListener((reachableNode) => {
      if (solver.isInstanceOf[Nothing]) if (!reachedForwardNodes.add(reachableNode)) reachedForwardNodeCollisions += 1
      else if (!reachedBackwardNodes.add(reachableNode)) reachedBackwardNodeCollisions += 1
    })
  }

  private def increaseMethod(method: Nothing, map: Nothing): Unit = {
    var i = map.get(method)
    if (i == null) i = new Nothing(0)
    map.put(method, {
      i += 1; i
    })
  }

  @Override def registerFieldWritePOI(key: Nothing): Unit = {
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
    if (COUNT_TOP_METHODS) {
      s += topMostMethods(forwardFieldMethodsRules, "forward field")
      s += topMostMethods(forwardCallMethodsRules, "forward call")
      if (!backwardCallMethodsRules.isEmpty) {
        s += topMostMethods(backwardFieldMethodsRules, "backward field")
        s += topMostMethods(backwardCallMethodsRules, "backward call")
      }
    }
    s += computeMetrics
    s += "\n"
    s
  }

  private def topMostMethods(fieldMethodsRules: Nothing, system: Nothing) = {
    val sootMethodIntegerMap = AdvancedBoomerangStats.sortByValues(fieldMethodsRules)
    var i = 0
    var s = ""
    import scala.collection.JavaConversions._
    for (e <- sootMethodIntegerMap.entrySet) {
      if ( {
        i += 1; i
      } > 11) break //todo: break is not supported
      s += String.format("%s. most %s visited Method(%sx): %s\n", i, system, e.getValue, e.getKey)
    }
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

  @Override def terminated(query: Nothing, forwardBoomerangResults: Nothing): Unit = {

    // TODO Auto-generated method stub
  }

  @Override def terminated(query: Nothing, backwardBoomerangResults: Nothing): Unit = {

    // TODO Auto-generated method stub
  }
}