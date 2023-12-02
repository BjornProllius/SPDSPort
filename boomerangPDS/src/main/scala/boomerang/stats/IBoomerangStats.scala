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
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

trait IBoomerangStats[W <: Weight] {
  def registerSolver(key: Query, solver: AbstractBoomerangSolver[W]): Unit

  def registerFieldWritePOI(key: WeightedBoomerang[W]#FieldWritePOI): Unit

  def getCallVisitedMethods(): Set[Method]

  def getForwardReachesNodes(): Collection[_ <: Node[ControlFlowGraph.Edge, Val]]

  def terminated(query: ForwardQuery, forwardBoomerangResults: ForwardBoomerangResults[W]): Unit

  def terminated(query: BackwardQuery, backwardBoomerangResults: BackwardBoomerangResults[W]): Unit
}