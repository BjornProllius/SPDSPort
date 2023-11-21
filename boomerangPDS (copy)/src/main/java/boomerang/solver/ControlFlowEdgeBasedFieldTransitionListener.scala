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
import boomerang.scene.Val
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAUpdateListener

abstract class ControlFlowEdgeBasedFieldTransitionListener[W <: Weight](private val cfgEdge: Nothing) extends Nothing {
  def getCfgEdge: Nothing = cfgEdge

  @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    onAddedTransition(t)
  }

  def onAddedTransition(t: Nothing): Unit

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (cfgEdge == null) 0
    else cfgEdge.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ControlFlowEdgeBasedFieldTransitionListener[_ <: Nothing]]
    if (cfgEdge == null) if (other.cfgEdge != null) return false
    else if (!cfgEdge.equals(other.cfgEdge)) return false
    true
  }
}