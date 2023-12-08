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

import boomerang.scene.{ControlFlowGraph, Field, Method, Val}
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.WPAUpdateListener

abstract class MethodBasedFieldTransitionListener[W <: Weight](val method: Method)
    extends WPAUpdateListener[Field, INode[Node[ControlFlowGraph.Edge, Val]], W] {

  def getMethod(): Method = method

  override def onWeightAdded(
      t: Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]],
      w: W,
      aut: WeightedPAutomaton[Field, INode[Node[ControlFlowGraph.Edge, Val]], W]): Unit = {
    onAddedTransition(t)
  }

  def onAddedTransition(t: Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]]): Unit
}