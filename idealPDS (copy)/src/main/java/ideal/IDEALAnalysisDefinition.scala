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
package ideal

import boomerang.BoomerangOptions
import boomerang.DefaultBoomerangOptions
import boomerang.WeightedForwardQuery
import boomerang.debugger.Debugger
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Val
import java.util
import sync.pds.solver.WeightFunctions
import wpds.impl.Weight

abstract class IDEALAnalysisDefinition[W <: Weight] {
  /**
   * This function generates the seed. Each (reachable) statement of the analyzed code is visited.
   * To place a seed, a pair of access graph and an edge function must be specified. From this node
   * the analysis starts its analysis.
   *
   * @param method
   * @param stmt The statement over which is iterated over
   * @return
   */
  def generate(stmt: Nothing): Nothing

  /**
   * This function must generate and return the AnalysisEdgeFunctions that are used for the
   * analysis. As for standard IDE in Heros, the edge functions for normal-, call-, return- and
   * call-to-return flows have to be specified.
   */
  def weightFunctions: Nothing

  def callGraph: Nothing

  def enableStrongUpdates = true

  def toString: Nothing = {
    val str = "====== IDEal Analysis Options ======"
    // str += "\nEdge Functions:\t\t" + edgeFunctions();
    // str += "\nDebugger Class:\t\t" + debugger();
    // str += "\nAnalysisBudget(sec):\t" + (analysisBudgetInSeconds());
    // str += "\nStrong Updates:\t\t" + (enableStrongUpdates() ? "ENABLED" : "DISABLED");
    // str += "\nAliasing:\t\t" + (enableAliasing() ? "ENABLED" : "DISABLED");
    // str += "\nNull POAs:\t\t" + (enableNullPointOfAlias() ? "ENABLED" : "DISABLED");
    // str += "\n" + boomerangOptions();
    str
  }

  def debugger(idealSeedSolver: Nothing): Nothing

  def boomerangOptions: Nothing = new Nothing() {
    @Override def getStaticFieldStrategy: Nothing = return StaticFieldStrategy.FLOW_SENSITIVE

    @Override def allowMultipleQueries: Boolean = return true
  }

  def getResultHandler = new Nothing

  protected def getDataFlowScope: Nothing
}