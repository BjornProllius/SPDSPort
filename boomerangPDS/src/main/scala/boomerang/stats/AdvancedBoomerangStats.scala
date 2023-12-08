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
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{Field, Method, Val}
import boomerang.solver.{AbstractBoomerangSolver, BackwardBoomerangSolver, ForwardBoomerangSolver}
import com.google.common.collect.{Maps, Sets}

import scala.collection.mutable
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.{Rule, Transition, Weight}
import wpds.interfaces.{Location, State}

class AdvancedBoomerangStats[W <: Weight] extends IBoomerangStats[W] {

  private var queries: Map[Query, AbstractBoomerangSolver[W]] = Maps.newHashMap()
  private var globalFieldTransitions: Set[WeightedTransition[Field, INode[Node[Edge, Val]], W]] = Sets.newHashSet()
  private var fieldTransitionCollisions: Int = 0
  private var globalCallTransitions: Set[WeightedTransition[Edge, INode[Val], W]] = Sets.newHashSet()
  private var callTransitionCollisions: Int = 0
  private var globalFieldRules: Set[Rule[Field, INode[Node[Edge, Val]], W]] = Sets.newHashSet()
  private var fieldRulesCollisions: Int = 0
  private var globalCallRules: Set[Rule[Edge, INode[Val], W]] = Sets.newHashSet()
  private var callRulesCollisions: Int = 0
  private var reachedForwardNodes: Set[Node[Edge, Val]] = Sets.newHashSet()
  private var reachedForwardNodeCollisions: Int = 0

  private var reachedBackwardNodes: Set[Node[Edge, Val]] = Sets.newHashSet()
  private var reachedBackwardNodeCollisions: Int = 0
  private var callVisitedMethods: Set[Method] = Sets.newHashSet()
  private var fieldVisitedMethods: Set[Method] = Sets.newHashSet()
  private var arrayFlows: Int = 0
  private var staticFlows: Int = 0
  private var COUNT_TOP_METHODS: Boolean = false
  private var backwardFieldMethodsRules: Map[String, Int] = new TreeMap()
  private var backwardCallMethodsRules: Map[String, Int] = new TreeMap()

  private var forwardFieldMethodsRules: Map[String, Int] = new TreeMap()
  private var forwardCallMethodsRules: Map[String, Int] = new TreeMap()

  def sortByValues[K](map: Map[K, Int]): Map[K, Int] = {
    val valueComparator: Comparator[K] = (k1, k2) => {
      if (map(k2) > map(k1)) 1
      else -1
    }
    val sortedByValues: Map[K, Int] = new TreeMap[K, Int](valueComparator)
    sortedByValues.putAll(map)
    sortedByValues
  }

  override def registerSolver(key: Query, solver: AbstractBoomerangSolver[W]): Unit = {
    if (queries.contains(key)) {
      return
    }
    queries.put(key, solver)

    solver.getFieldAutomaton().registerListener((t, w, aut) => {
      if (!globalFieldTransitions.add(new WeightedTransition(t, w))) {
        fieldTransitionCollisions += 1
      }
      fieldVisitedMethods.add(t.getStart().fact().stmt().getMethod())
      if (t.getLabel().isInstanceOf[ArrayField]) {
        arrayFlows += 1
      }
    })

    solver.getCallAutomaton().registerListener((t, w, aut) => {
      if (!globalCallTransitions.add(new WeightedTransition(t, w))) {
        callTransitionCollisions += 1
      }
      callVisitedMethods.add(t.getLabel().getMethod())
      if (t.getStart().fact().isStatic()) {
        staticFlows += 1
      }
    })

    solver.getFieldPDS().registerUpdateListener(rule => {
      if (!globalFieldRules.add(rule)) {
        fieldRulesCollisions += 1
      } else if (COUNT_TOP_METHODS) {
        increaseMethod(
          rule.getS1().fact().stmt().getMethod().toString(),
          if (solver.isInstanceOf[BackwardBoomerangSolver]) backwardFieldMethodsRules else forwardFieldMethodsRules
        )
      }
    })

    solver.getCallPDS().registerUpdateListener(rule => {
      if (!globalCallRules.add(rule)) {
        callRulesCollisions += 1
      } else if (COUNT_TOP_METHODS) {
        increaseMethod(
          rule.getL1().getMethod().toString(),
          if (solver.isInstanceOf[BackwardBoomerangSolver]) backwardCallMethodsRules else forwardCallMethodsRules
        )
      }
    })

    solver.registerListener(reachableNode => {
      if (solver.isInstanceOf[ForwardBoomerangSolver]) {
        if (!reachedForwardNodes.add(reachableNode)) {
          reachedForwardNodeCollisions += 1
        }
      } else {
        if (!reachedBackwardNodes.add(reachableNode)) {
          reachedBackwardNodeCollisions += 1
        }
      }
    })
  }

  private def increaseMethod(method: String, map: mutable.Map[String, Int]): Unit = {
    val i = map.getOrElse(method, 0)
    map.put(method, i + 1)
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
    s += f"Queries (Forward/Backward/Total): \t\t $forwardQuery/$backwardQuery/${queries.keySet.size}\n"
    s += f"Visited Methods (Field/Call): \t\t ${fieldVisitedMethods.size}/${callVisitedMethods.size}\n"
    s += f"Reached Forward Nodes(Collisions): \t\t ${reachedForwardNodes.size} ($reachedForwardNodeCollisions)\n"
    s += f"Reached Backward Nodes(Collisions): \t\t ${reachedBackwardNodes.size} ($reachedBackwardNodeCollisions)\n"
    s += f"Global Field Rules(Collisions): \t\t ${globalFieldRules.size} ($fieldRulesCollisions)\n"
    s += f"Global Field Transitions(Collisions): \t\t ${globalFieldTransitions.size} ($fieldTransitionCollisions)\n"
    s += f"Global Call Rules(Collisions): \t\t ${globalCallRules.size} ($callRulesCollisions)\n"
    s += f"Global Call Transitions(Collisions): \t\t ${globalCallTransitions.size} ($callTransitionCollisions)\n"
    s += f"Special Flows (Static/Array): \t\t $staticFlows(${globalCallTransitions.size})/$arrayFlows(${globalFieldTransitions.size})\n"
    if (COUNT_TOP_METHODS) {
      s += topMostMethods(forwardFieldMethodsRules, "forward field")
      s += topMostMethods(forwardCallMethodsRules, "forward call")
      if (backwardCallMethodsRules.nonEmpty) {
        s += topMostMethods(backwardFieldMethodsRules, "backward field")
        s += topMostMethods(backwardCallMethodsRules, "backward call")
      }
    }
    s += computeMetrics()
    s += "\n"
    s
  }

  private def topMostMethods(fieldMethodsRules: mutable.Map[String, Int], system: String): String = {
    val sootMethodIntegerMap = sortByValues(fieldMethodsRules)
    var i = 0
    var s = ""
    for ((key, value) <- sootMethodIntegerMap) {
      i += 1
      if (i > 11) return s
      s += f"$i. most $system visited Method(${value}x): $key\n"
    }
    s
  }

  override def getCallVisitedMethods: Set[Method] = {
    callVisitedMethods.toSet
  }

  private def computeMetrics(): String = {
    var min = Int.MaxValue
    var totalReached = 0
    var max = 0
    var maxQuery: Query = null
    for (q <- queries.keySet) {
      val size = queries(q).getReachedStates.size
      totalReached += size
      min = Math.min(size, min)
      if (size > max) {
        maxQuery = q
      }
      max = Math.max(size, max)
    }
    val average = totalReached.toFloat / queries.keySet.size
    var s = f"Reachable nodes (Min/Avg/Max): \t\t$min/$average/$max\n"
    s += f"Maximal Query: \t\t$maxQuery\n"
    s
  }

  private class WeightedTransition[X <: Location, Y <: State, W](val t: Transition[X, Y], val w: W) {
    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (t == null) 0 else t.hashCode())
      result = prime * result + (if (w == null) 0 else w.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case that: WeightedTransition[X, Y, W] =>
        (that canEqual this) &&
          t == that.t &&
          w == that.w
      case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedTransition[X, Y, W]]
  }

  override def getForwardReachesNodes: Collection[_ <: Node[Edge, Val]] = {
    val res = mutable.HashSet[Node[Edge, Val]]()
    for (q <- queries.keySet) {
      if (q.isInstanceOf[ForwardQuery]) res ++= queries(q).getReachedStates
    }
    res
  }

  override def terminated(query: ForwardQuery, forwardBoomerangResults: ForwardBoomerangResults[W]): Unit = {
    // TODO Auto-generated method stub
  }

  override def terminated(query: BackwardQuery, backwardBoomerangResults: BackwardBoomerangResults[W]): Unit = {
    // TODO Auto-generated method stub
  }
}