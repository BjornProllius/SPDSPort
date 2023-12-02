/**
  * ******************************************************************************
  * Copyright (c) 2018
  * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
  * available under the terms of the Eclipse Public License 2.0 which is available at
  * http://www.eclipse.org/legal/epl-2.0.
  *
  * SPDX-License-Identifier: EPL-2.0
  *
  * Contributors: Johannes Spaeth - initial API and implementation
  * ******************************************************************************
  */
package ideal

import boomerang.{BoomerangOptions, DefaultBoomerangOptions, WeightedForwardQuery}
import boomerang.debugger.Debugger
import boomerang.scene.{CallGraph, ControlFlowGraph, DataFlowScope}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import sync.pds.solver.WeightFunctions
import wpds.impl.Weight

import scala.collection.JavaConverters._

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
  def generate(stmt: ControlFlowGraph.Edge): Collection[WeightedForwardQuery[W]]

  /**
    * This function must generate and return the AnalysisEdgeFunctions that are used for the
    * analysis. As for standard IDE in Heros, the edge functions for normal-, call-, return- and
    * call-to-return flows have to be specified.
    */
  def weightFunctions(): WeightFunctions[Edge, Val, Edge, W]

  def callGraph(): CallGraph

  def enableStrongUpdates(): Boolean = true

  override def toString: String = {
    s"====== IDEal Analysis Options ======"
    // Add more details if needed
  }

  def debugger(idealSeedSolver: IDEALSeedSolver[W]): Debugger[W]

  def boomerangOptions(): BoomerangOptions = new DefaultBoomerangOptions() {
    override def getStaticFieldStrategy: StaticFieldStrategy = StaticFieldStrategy.FLOW_SENSITIVE

    override def allowMultipleQueries(): Boolean = true
  }

  def getResultHandler(): IDEALResultHandler[W] = new IDEALResultHandler[W]()

  protected def getDataFlowScope(): DataFlowScope
}
