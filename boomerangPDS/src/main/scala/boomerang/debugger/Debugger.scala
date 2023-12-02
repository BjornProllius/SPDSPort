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
package boomerang.debugger

import boomerang.{ForwardQuery, ObservableICFG}
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.scene.{Method, Statement}
import boomerang.solver.ForwardBoomerangSolver
import wpds.impl.Weight

import scala.collection.Set
import scala.collection.immutable.Map

class Debugger[W <: Weight] {

  def done(
      icfg: ObservableICFG[Statement, Method],
      cfg: ObservableControlFlowGraph,
      visitedMethods: Set[Method],
      queryToSolvers: Map[ForwardQuery, ForwardBoomerangSolver[W]]): Unit = {
    // TODO Auto-generated method stub
  }
}