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

class GeneratedState[L, N](private var node: Nothing, private var loc: N) extends Nothing {
  @Override def fact: L = {
    node.fact
    // throw new RuntimeException("System internal state");
  }

  def node: Nothing = node

  def location: N = loc

  @Override def toString: Nothing = node + " " + loc

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (loc == null) 0
    else loc.hashCode)
    result = prime * result + (if (node == null) 0
    else node.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[GeneratedState[_, _]]
    if (loc == null) if (other.loc != null) return false
    else if (!loc.equals(other.loc)) return false
    if (node == null) if (other.node != null) return false
    else if (!node.equals(other.node)) return false
    true
  }
}