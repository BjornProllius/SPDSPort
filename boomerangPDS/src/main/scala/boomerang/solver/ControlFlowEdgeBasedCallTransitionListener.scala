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

import boomerang.scene.{ControlFlowGraph, Val}
import sync.pds.solver.nodes.INode
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.WPAUpdateListener

abstract class ControlFlowEdgeBasedCallTransitionListener[W <: Weight](edge: ControlFlowGraph.Edge)
    extends WPAUpdateListener[ControlFlowGraph.Edge, INode[Val], W] {

  def getControlFlowEdge(): ControlFlowGraph.Edge = edge

  override def onWeightAdded(
      t: Transition[ControlFlowGraph.Edge, INode[Val]],
      w: W,
      aut: WeightedPAutomaton[ControlFlowGraph.Edge, INode[Val], W]): Unit = {
    onAddedTransition(t, w)
  }

  def onAddedTransition(t: Transition[ControlFlowGraph.Edge, INode[Val]], w: W): Unit

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (edge == null) 0 else edge.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: ControlFlowEdgeBasedCallTransitionListener[_] =>
      (that canEqual this) && edge == that.edge
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ControlFlowEdgeBasedCallTransitionListener[_]]
}