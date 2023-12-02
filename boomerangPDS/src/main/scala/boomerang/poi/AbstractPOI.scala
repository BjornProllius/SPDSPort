import boomerang.scene.ControlFlowGraph.Edge

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
abstract class AbstractPOI[Statement, Val, Field](cfgEdge: Edge, baseVar: Val, field: Field, storedVar: Val) extends PointOfIndirection[Statement, Val, Field] {

  def getBaseVar: Val = baseVar

  def getField: Field = field

  def getStoredVar: Val = storedVar

  def getCfgEdge: Edge = cfgEdge

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (field == null) 0 else field.hashCode)
    result = prime * result + (if (baseVar == null) 0 else baseVar.hashCode)
    result = prime * result + (if (storedVar == null) 0 else storedVar.hashCode)
    result = prime * result + (if (cfgEdge == null) 0 else cfgEdge.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: AbstractPOI[_, _, _] =>
      (this.field == null && that.field == null || this.field != null && this.field.equals(that.field)) &&
      (this.baseVar == null && that.baseVar == null || this.baseVar != null && this.baseVar.equals(that.baseVar)) &&
      (this.storedVar == null && that.storedVar == null || this.storedVar != null && this.storedVar.equals(that.storedVar)) &&
      (this.cfgEdge == null && that.cfgEdge == null || this.cfgEdge != null && this.cfgEdge.equals(that.cfgEdge))
    case _ => false
  }

  override def toString: String = "POI:" + cfgEdge.toString
}