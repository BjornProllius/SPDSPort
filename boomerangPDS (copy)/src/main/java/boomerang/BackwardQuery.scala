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

import boomerang.scene.ControlFlowGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val

object BackwardQuery {
  def make(edge: Nothing, variable: Nothing) = new BackwardQuery(edge, variable)
}

class BackwardQuery protected(edge: Nothing, variable: Nothing) extends Nothing(edge, variable) {
  @Override def toString: Nothing = "BackwardQuery: " + super.toString
}