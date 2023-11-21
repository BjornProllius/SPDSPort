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
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import sync.pds.solver.nodes.INode
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAUpdateListener

abstract class ControlFlowEdgeBasedCallTransitionListener[W <: Weight](private val edge: Nothing) extends Nothing {
  def getControlFlowEdge: Nothing = edge

  @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    onAddedTransition(t, w)
  }

  def onAddedTransition(t: Nothing, w: W): Unit

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (edge == null) 0
    else edge.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ControlFlowEdgeBasedCallTransitionListener[_ <: Nothing]]
    if (edge == null) if (other.edge != null) return false
    else if (!edge.equals(other.edge)) return false
    true
  }
}