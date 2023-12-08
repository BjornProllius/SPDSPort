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

class ExclusionNode[Stmt, Fact, Location](stmt: Stmt, variable: Fact, private var exclusion: Location) extends Nothing(stmt, variable) {
  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (exclusion == null) 0
    else exclusion.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ExclusionNode[_, _, _]]
    if (exclusion == null) if (other.exclusion != null) return false
    else if (!exclusion.equals(other.exclusion)) return false
    true
  }

  def exclusion: Location = exclusion
}