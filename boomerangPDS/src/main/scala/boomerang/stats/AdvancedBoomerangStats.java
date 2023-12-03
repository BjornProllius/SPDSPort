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

  











}

  


  private void increaseMethod(String method, Map<String, Integer> map) {
    Integer i = map.get(method);
    if (i == null) {
      i = new Integer(0);
    }
    map.put(method, ++i);
  }

  @Override
  public void registerFieldWritePOI(WeightedBoomerang<W>.FieldWritePOI key) {}

  public String toString() {
    String s = "=========== Boomerang Stats =============\n";
    int forwardQuery = 0;
    int backwardQuery = 0;
    for (Query q : queries.keySet()) {
      if (q instanceof ForwardQuery) {
        forwardQuery++;
      } else backwardQuery++;
    }
    s +=
        String.format(
            "Queries (Forward/Backward/Total): \t\t %s/%s/%s\n",
            forwardQuery, backwardQuery, queries.keySet().size());
    s +=
        String.format(
            "Visited Methods (Field/Call): \t\t %s/%s\n",
            fieldVisitedMethods.size(), callVisitedMethods.size());
    s +=
        String.format(
            "Reached Forward Nodes(Collisions): \t\t %s (%s)\n",
            reachedForwardNodes.size(), reachedForwardNodeCollisions);
    s +=
        String.format(
            "Reached Backward Nodes(Collisions): \t\t %s (%s)\n",
            reachedBackwardNodes.size(), reachedBackwardNodeCollisions);
    s +=
        String.format(
            "Global Field Rules(Collisions): \t\t %s (%s)\n",
            globalFieldRules.size(), fieldRulesCollisions);
    s +=
        String.format(
            "Global Field Transitions(Collisions): \t\t %s (%s)\n",
            globalFieldTransitions.size(), fieldTransitionCollisions);
    s +=
        String.format(
            "Global Call Rules(Collisions): \t\t %s (%s)\n",
            globalCallRules.size(), callRulesCollisions);
    s +=
        String.format(
            "Global Call Transitions(Collisions): \t\t %s (%s)\n",
            globalCallTransitions.size(), callTransitionCollisions);
    s +=
        String.format(
            "Special Flows (Static/Array): \t\t %s(%s)/%s(%s)\n",
            staticFlows, globalCallTransitions.size(), arrayFlows, globalFieldTransitions.size());
    if (COUNT_TOP_METHODS) {
      s += topMostMethods(forwardFieldMethodsRules, "forward field");
      s += topMostMethods(forwardCallMethodsRules, "forward call");

      if (!backwardCallMethodsRules.isEmpty()) {
        s += topMostMethods(backwardFieldMethodsRules, "backward field");
        s += topMostMethods(backwardCallMethodsRules, "backward call");
      }
    }
    s += computeMetrics();
    s += "\n";
    return s;
  }

  private String topMostMethods(Map<String, Integer> fieldMethodsRules, String system) {
    Map<String, Integer> sootMethodIntegerMap = sortByValues(fieldMethodsRules);
    int i = 0;
    String s = "";
    for (Map.Entry<String, Integer> e : sootMethodIntegerMap.entrySet()) {
      if (++i > 11) break;
      s +=
          String.format(
              "%s. most %s visited Method(%sx): %s\n", i, system, e.getValue(), e.getKey());
    }
    return s;
  }

  @Override
  public Set<Method> getCallVisitedMethods() {
    return Sets.newHashSet(callVisitedMethods);
  }

  private String computeMetrics() {
    int min = Integer.MAX_VALUE;
    int totalReached = 0;
    int max = 0;
    Query maxQuery = null;
    for (Query q : queries.keySet()) {
      int size = queries.get(q).getReachedStates().size();
      totalReached += size;
      min = Math.min(size, min);
      if (size > max) {
        maxQuery = q;
      }
      max = Math.max(size, max);
    }
    float average = ((float) totalReached) / queries.keySet().size();
    String s = String.format("Reachable nodes (Min/Avg/Max): \t\t%s/%s/%s\n", min, average, max);
    s += String.format("Maximal Query: \t\t%s\n", maxQuery);
    return s;
  }

  private static class WeightedTransition<X extends Location, Y extends State, W> {
    final Transition<X, Y> t;
    final W w;

    public WeightedTransition(Transition<X, Y> t, W w) {
      this.t = t;
      this.w = w;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((t == null) ? 0 : t.hashCode());
      result = prime * result + ((w == null) ? 0 : w.hashCode());
      return result;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;
      WeightedTransition other = (WeightedTransition) obj;
      if (t == null) {
        if (other.t != null) return false;
      } else if (!t.equals(other.t)) return false;
      if (w == null) {
        if (other.w != null) return false;
      } else if (!w.equals(other.w)) return false;
      return true;
    }
  }

  @Override
  public Collection<? extends Node<Edge, Val>> getForwardReachesNodes() {
    Set<Node<Edge, Val>> res = Sets.newHashSet();
    for (Query q : queries.keySet()) {
      if (q instanceof ForwardQuery) res.addAll(queries.get(q).getReachedStates());
    }
    return res;
  }

  @Override
  public void terminated(ForwardQuery query, ForwardBoomerangResults<W> forwardBoomerangResults) {
    // TODO Auto-generated method stub

  }

  @Override
  public void terminated(
      BackwardQuery query, BackwardBoomerangResults<W> backwardBoomerangResults) {
    // TODO Auto-generated method stub

  }
}
