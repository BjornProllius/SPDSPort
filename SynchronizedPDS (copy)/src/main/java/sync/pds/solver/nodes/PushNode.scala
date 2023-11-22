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
package sync.pds.solver.nodes

import sync.pds.solver.SyncPDSSolver.PDSSystem

class PushNode[Stmt, Fact, Location](stmt: Stmt, variable: Fact, private var location: Location, private var system: Nothing) extends Nothing(stmt, variable) {
  def system: Nothing = system

  def location: Location = location

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (location == null) 0
    else location.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[PushNode[_, _, _]]
    if (location == null) if (other.location != null) return false
    else if (!location.equals(other.location)) return false
    true
  }

  @Override def toString: Nothing = super.toString + " Push " + location
}