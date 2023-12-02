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

import boomerang.{BackwardQuery, ForwardQuery, Query, WeightedBoomerang}
import boomerang.results.{BackwardBoomerangResults, ForwardBoomerangResults}
import boomerang.scene.{ControlFlowGraph, Method, Val}
import boomerang.solver.AbstractBoomerangSolver
import com.google.common.collect.{Maps, Sets}
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.WPAUpdateListener

import scala.collection.mutable

class SimpleBoomerangStats[W <: Weight] extends IBoomerangStats[W] {

  private val queries = Maps.newHashMap[Query, AbstractBoomerangSolver[W]]()
  private val callVisitedMethods = Sets.newHashSet[Method]()
  private val fieldVisitedMethods = Sets.newHashSet[Method]()

  override def registerSolver(key: Query, solver: AbstractBoomerangSolver[W]): Unit = {
    if (queries.containsKey(key)) {
      return
    }
    queries.put(key, solver)

    solver.getCallAutomaton.registerListener(new WPAUpdateListener[ControlFlowGraph.Edge, INode[Val], W] {
      override def onWeightAdded(t: Transition[ControlFlowGraph.Edge, INode[Val]], w: W, aut: WeightedPAutomaton[ControlFlowGraph.Edge, INode[Val], W]): Unit = {
        callVisitedMethods.add(t.getLabel.getMethod)
      }
    })
    solver.getFieldAutomaton.registerListener((t, w, aut) => fieldVisitedMethods.add(t.getStart.fact().stmt().getMethod))
  }

  override def registerFieldWritePOI(key: WeightedBoomerang[W]#FieldWritePOI): Unit = {}

  override def toString: String = {
    var s = "=========== Boomerang Stats =============\n"
    var forwardQuery = 0
    var backwardQuery = 0
    for (q <- queries.keySet) {
      if (q.isInstanceOf[ForwardQuery]) {
        forwardQuery += 1
      } else backwardQuery += 1
    }
    s += String.format("Queries (Forward/Backward/Total): \t\t %s/%s/%s\n", forwardQuery, backwardQuery, queries.keySet.size)
    s += String.format("Visited Methods (Field/Call): \t\t %s/%s/(%s/%s)\n", fieldVisitedMethods.size, callVisitedMethods.size, Sets.difference(fieldVisitedMethods, callVisitedMethods).size, Sets.difference(callVisitedMethods, fieldVisitedMethods).size)
    s += "\n"
    s
  }

  override def getForwardReachesNodes: Collection[_ <: Node[ControlFlowGraph.Edge, Val]] = {
    val res = Sets.newHashSet[Node[ControlFlowGraph.Edge, Val]]()
    for (q <- queries.keySet) {
      if (q.isInstanceOf[ForwardQuery]) res.addAll(queries.get(q).getReachedStates)
    }
    res
  }

  override def getCallVisitedMethods: Set[Method] = Sets.newHashSet(callVisitedMethods)

  override def terminated(query: ForwardQuery, forwardBoomerangResults: ForwardBoomerangResults[W]): Unit = {}

  override def terminated(query: BackwardQuery, backwardBoomerangResults: BackwardBoomerangResults[W]): Unit = {}
}