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
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

/** Created by johannesspath on 06.12.17. */
trait IBoomerangStats[W <: Weight] {
  def registerSolver(key: Nothing, solver: Nothing): Unit

  def registerFieldWritePOI(key: Nothing): Unit

  def getCallVisitedMethods: Nothing

  def getForwardReachesNodes: Nothing

  def terminated(query: Nothing, forwardBoomerangResults: Nothing): Unit

  def terminated(query: Nothing, backwardBoomerangResults: Nothing): Unit
}