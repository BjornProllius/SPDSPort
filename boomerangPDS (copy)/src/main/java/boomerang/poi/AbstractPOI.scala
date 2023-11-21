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
package boomerang.poi

import boomerang.scene.ControlFlowGraph.Edge

abstract class AbstractPOI[Statement, Val, Field](private var cfgEdge: Nothing, private val baseVar: Val, private val field: Field, private val storedVar: Val) extends Nothing {
  def getBaseVar: Val = baseVar

  def getField: Field = field

  def getStoredVar: Val = storedVar

  def getCfgEdge: Nothing = cfgEdge

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (field == null) 0
    else field.hashCode)
    result = prime * result + (if (baseVar == null) 0
    else baseVar.hashCode)
    result = prime * result + (if (storedVar == null) 0
    else storedVar.hashCode)
    result = prime * result + (if (cfgEdge == null) 0
    else cfgEdge.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[AbstractPOI[_, _, _]]
    if (field == null) if (other.field != null) return false
    else if (!field.equals(other.field)) return false
    if (baseVar == null) if (other.baseVar != null) return false
    else if (!baseVar.equals(other.baseVar)) return false
    if (storedVar == null) if (other.storedVar != null) return false
    else if (!storedVar.equals(other.storedVar)) return false
    if (cfgEdge == null) if (other.cfgEdge != null) return false
    else if (!cfgEdge.equals(other.cfgEdge)) return false
    true
  }

  @Override def toString: Nothing = "POI:" + cfgEdge.toString
}