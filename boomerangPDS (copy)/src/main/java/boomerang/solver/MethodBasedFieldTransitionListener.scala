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
package boomerang.solver

import boomerang.scene.ControlFlowGraph
import boomerang.scene.Field
import boomerang.scene.Method
import boomerang.scene.Val
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAUpdateListener

abstract class MethodBasedFieldTransitionListener[W <: Weight](private val method: Nothing) extends Nothing {
  def getMethod: Nothing = method

  @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    onAddedTransition(t)
  }

  def onAddedTransition(t: Nothing): Unit
}