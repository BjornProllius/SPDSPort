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

import wpds.interfaces.State

class Node[Stmt, Fact](protected val stmt: Stmt, protected val variable: Fact) extends Nothing {
  private var hashCode = 0

  def stmt: Stmt = stmt

  def fact: Fact = variable

  @Override def hashCode: Int = {
    if (hashCode != 0) return hashCode
    val prime = 31
    var result = 1
    result = prime * result + (if (stmt == null) 0
    else stmt.hashCode)
    result = prime * result + (if (variable == null) 0
    else variable.hashCode)
    hashCode = result
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Node[_, _]]
    if (stmt == null) if (other.stmt != null) return false
    else if (!stmt.equals(other.stmt)) return false
    if (variable == null) if (other.variable != null) return false
    else if (!variable.equals(other.variable)) return false
    true
  }

  @Override def toString: Nothing = "(" + variable + "," + stmt + ")"
}