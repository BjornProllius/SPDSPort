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

class CallPopNode[Location, Stmt](location: Location, system: Nothing, private val returnSite: Stmt) extends Nothing(location, system) {
  def getReturnSite: Stmt = returnSite

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (returnSite == null) 0
    else returnSite.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[CallPopNode[_, _]]
    if (returnSite == null) if (other.returnSite != null) return false
    else if (!returnSite.equals(other.returnSite)) return false
    true
  }
}