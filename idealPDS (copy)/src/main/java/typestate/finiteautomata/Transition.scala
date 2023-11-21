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
package typestate.finiteautomata

object Transition {
  private var instance: Transition = null

  def identity: Transition = {
    if (instance == null) instance = new Transition("ID -> ID")
    instance
  }
}

class Transition extends Nothing {
  final private var from: Nothing = null
  final private var to: Nothing = null
  final private var rep: Nothing = null

  def this(from: Nothing, to: Nothing) {
    this()
    this.from = from
    this.to = to
    this.rep = null
  }

  def this(rep: Nothing) {
    this()
    this.from = null
    this.to = null
    this.rep = rep
  }

  def from: Nothing = from

  def to: Nothing = to

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (from == null) 0
    else from.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result = prime * result + (if (to == null) 0
    else to.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Transition]
    if (from == null) if (other.from != null) return false
    else if (!from.equals(other.from)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    if (to == null) if (other.to != null) return false
    else if (!to.equals(other.to)) return false
    true
  }

  def toString: Nothing = {
    if (rep != null) return rep
    "" + from + " -> " + to
  }
}