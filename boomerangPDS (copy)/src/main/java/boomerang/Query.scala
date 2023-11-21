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

import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Type
import boomerang.scene.Val
import sync.pds.solver.nodes.Node

abstract class Query(private val cfgEdge: Nothing, private val variable: Nothing) {
  def asNode = new Nothing(cfgEdge, variable)

  @Override def toString: Nothing = new Nothing(cfgEdge, variable).toString

  def cfgEdge: Nothing = cfgEdge

  def `var`: Nothing = variable

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (cfgEdge == null) 0
    else cfgEdge.hashCode)
    result = prime * result + (if (variable == null) 0
    else variable.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (obj.getClass ne this.getClass) return false
    val other = obj.asInstanceOf[Query]
    if (cfgEdge == null) if (other.cfgEdge != null) return false
    else if (!cfgEdge.equals(other.cfgEdge)) return false
    if (variable == null) if (other.variable != null) return false
    else if (!variable.equals(other.variable)) return false
    true
  }

  def getType: Nothing = {
    if (variable.isInstanceOf[Nothing]) {
      val allocVal = variable.asInstanceOf[Nothing]
      return allocVal.getAllocVal.getType
    }
    variable.getType
  }
}