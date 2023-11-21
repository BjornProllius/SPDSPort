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
package tests

import wpds.impl.Weight
import wpds.interfaces.Location

object MinSemiring {
  private var one: MinSemiring = null

  def one[N <: Location]: MinSemiring = {
    if (one == null) one = new MinSemiring(0) {
      @Override override def toString = "<ONE>"
    }
    one
  }

  private var zero: MinSemiring = null

  def zero[N <: Location]: MinSemiring = {
    if (zero == null) zero = new MinSemiring(110000) {
      @Override override def toString = "<ZERO>"
    }
    zero
  }
}

class MinSemiring private extends Nothing {
  private[tests] val i = 0

  def this(i: Int) {
    this()
    this.i = i
  }

  @Override def extendWith(other: Nothing): Nothing = {
    if (other.equals(MinSemiring.one)) return this
    if (this == MinSemiring.one) return other
    val o = other.asInstanceOf[MinSemiring]
    new MinSemiring(o.i + i)
  }

  @Override def combineWith(other: Nothing): Nothing = {
    if (other.equals(MinSemiring.zero)) return this
    if (this == MinSemiring.zero) return other
    val o = other.asInstanceOf[MinSemiring]
    new MinSemiring(Math.min(o.i, i))
  }

  @Override def toString: Nothing = Integer.toString(i)

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + i
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[MinSemiring]
    if (i != other.i) return false
    true
  }
}