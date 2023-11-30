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

import boomerang.scene.{AllocVal, ControlFlowGraph, Type, Val}
import sync.pds.solver.nodes.Node

abstract class Query(cfgEdge: ControlFlowGraph.Edge, variable: Val) {

  def asNode(): Node[ControlFlowGraph.Edge, Val] = new Node(cfgEdge, variable)

  override def toString: String = new Node(cfgEdge, variable).toString

  def cfgEdge(): ControlFlowGraph.Edge = cfgEdge

  def `var`(): Val = variable

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (cfgEdge == null) 0 else cfgEdge.hashCode)
    result = prime * result + (if (variable == null) 0 else variable.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Query =>
        if (this == other) return true
        if (cfgEdge == null) {
          if (other.cfgEdge != null) return false
        } else if (cfgEdge != other.cfgEdge) return false
        if (variable == null) {
          if (other.variable != null) return false
        } else if (variable != other.variable) return false
        true
      case _ => false
    }
  }

  def getType(): Type = {
    variable match {
      case allocVal: AllocVal => allocVal.getAllocVal.getType
      case _ => variable.getType
    }
  }
}