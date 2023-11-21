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

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import wpds.impl.Weight

class WeightedForwardQuery[W <: Weight](stmt: Nothing, variable: Nothing, private val weight: W) extends Nothing(stmt, variable) {
  def weight: W = weight
}