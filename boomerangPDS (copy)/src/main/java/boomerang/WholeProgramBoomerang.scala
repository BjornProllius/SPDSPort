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
package boomerang

import boomerang.scene.AnalysisScope
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Statement
import java.util
import java.util.Collections
import wpds.impl.Weight

abstract class WholeProgramBoomerang[W <: Weight](private var callGraph: Nothing, scope: Nothing, opts: Nothing) extends Nothing(cg, scope, opts) {
  private val reachableMethodCount = 0
  private val allocationSites = 0

  def this(cg: Nothing, scope: Nothing) {
    this(cg, scope, new Nothing)
  }

  def wholeProgramAnalysis(): Unit = {
    val before = System.currentTimeMillis
    val scope = new Nothing(callGraph) {
      @Override protected def generate(cfgEdge: Nothing): Nothing = {
        val stmt = cfgEdge.getStart
        if (stmt.isAssign) if (stmt.getRightOp.isNewExpr) return Collections.singleton(new Nothing(cfgEdge, stmt.getRightOp))
        Collections.emptySet
      }
    }
    import scala.collection.JavaConversions._
    for (s <- scope.computeSeeds) {
      solve(s.asInstanceOf[Nothing])
    }
    val after = System.currentTimeMillis
    System.out.println("Analysis Time (in ms):\t" + (after - before))
    System.out.println("Analyzed methods:\t" + reachableMethodCount)
    System.out.println("Total solvers:\t" + this.getSolvers.size)
    System.out.println("Allocation Sites:\t" + allocationSites)
    System.out.println(options.statsFactory)
  }

  @Override protected def backwardSolve(query: Nothing): Unit = {
  }
}