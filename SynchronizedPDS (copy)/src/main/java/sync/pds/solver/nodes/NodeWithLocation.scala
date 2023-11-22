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

class NodeWithLocation[Stmt, Fact, Location](stmt: Stmt, variable: Fact, private var loc: Location) extends Nothing {
  this.fact = new Nothing(stmt, variable)
  private var fact: Nothing = null

  @Override def fact: Nothing = fact

  def location: Location = loc

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (fact == null) 0
    else fact.hashCode)
    result = prime * result + (if (loc == null) 0
    else loc.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[NodeWithLocation[_, _, _]]
    if (fact == null) if (other.fact != null) return false
    else if (!fact.equals(other.fact)) return false
    if (loc == null) if (other.loc != null) return false
    else if (!loc.equals(other.loc)) return false
    true
  }

  @Override def toString: Nothing = fact + " loc: " + loc
}