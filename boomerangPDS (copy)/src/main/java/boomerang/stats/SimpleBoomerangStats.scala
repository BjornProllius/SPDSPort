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
import boomerang.scene.Method
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import java.util
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAUpdateListener

/** Created by johannesspath on 06.12.17. */
class SimpleBoomerangStats[W <: Weight] extends Nothing {
  private val queries = Maps.newHashMap
  private val callVisitedMethods = Sets.newHashSet
  private val fieldVisitedMethods = Sets.newHashSet

  @Override def registerSolver(key: Nothing, solver: Nothing): Unit = {
    if (queries.containsKey(key)) return
    queries.put(key, solver)
    solver.getCallAutomaton.registerListener(new Nothing() {
      @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
        callVisitedMethods.add(t.getLabel.getMethod)
      }
    })
    solver.getFieldAutomaton.registerListener((t, w, aut) => fieldVisitedMethods.add(t.getStart.fact.stmt.getMethod))
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
    s += String.format("Visited Methods (Field/Call): \t\t %s/%s/(%s/%s)\n", fieldVisitedMethods.size, callVisitedMethods.size, Sets.difference(fieldVisitedMethods, callVisitedMethods).size, Sets.difference(callVisitedMethods, fieldVisitedMethods).size)
    s += "\n"
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

  @Override def getCallVisitedMethods: Nothing = Sets.newHashSet(callVisitedMethods)

  @Override def terminated(query: Nothing, forwardBoomerangResults: Nothing): Unit = {
  }

  @Override def terminated(query: Nothing, backwardBoomerangResults: Nothing): Unit = {
  }
}