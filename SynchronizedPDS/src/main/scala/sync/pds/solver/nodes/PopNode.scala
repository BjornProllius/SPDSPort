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
import wpds.interfaces.State

class PopNode[Location](private var location: Location, private var system: Nothing) extends Nothing {
  def system: Nothing = system

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (location == null) 0
    else location.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[PopNode[_]]
    if (location == null) if (other.location != null) return false
    else if (!location.equals(other.location)) return false
    true
  }

  def location: Location = location

  @Override def toString: Nothing = "Pop " + location
}