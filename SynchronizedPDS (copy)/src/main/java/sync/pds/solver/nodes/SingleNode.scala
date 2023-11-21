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

class SingleNode[Fact](private var fact: Fact) extends Nothing {
  private var hashCode = 0

  @Override def hashCode: Int = {
    if (hashCode != 0) return hashCode
    val prime = 31
    var result = 1
    result = prime * result + (if (fact == null) 0
    else fact.hashCode)
    hashCode = result
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[SingleNode[_]]
    if (fact == null) if (other.fact != null) return false
    else if (!fact.equals(other.fact)) return false
    true
  }

  @Override def fact: Fact = fact

  @Override def toString: Nothing = fact.toString
}