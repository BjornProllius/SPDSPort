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

import boomerang.ForwardQuery
import boomerang.callgraph.ObservableICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.solver.ForwardBoomerangSolver
import java.util
import wpds.impl.Weight

class Debugger[W <: Weight] {
  def done(icfg: Nothing, cfg: Nothing, visitedMethods: Nothing, queryToSolvers: Nothing): Unit = {

    // TODO Auto-generated method stub
  }
}