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

object NumWeight {
  private var one: NumWeight = null

  def one[N <: Location]: NumWeight = {
    if (one == null) one = new NumWeight() {
      @Override override def toString = "<ONE>"

      @Override override def equals(obj: Nothing): Boolean = obj eq this
    }
    one
  }

  private var zero: NumWeight = null

  def zero[N <: Location]: NumWeight = {
    if (zero == null) zero = new NumWeight() {
      @Override override def toString = "<ZERO>"

      @Override override def equals(obj: Nothing): Boolean = obj eq this
    }
    zero
  }
}

class NumWeight private extends Nothing {
  private var i = 0

  def this(i: Int) {
    this()
    this.i = i
  }

  @Override def extendWith(other: Nothing): Nothing = {
    if (this == NumWeight.one) return other
    if (other.equals(NumWeight.one)) return this
    if (this == NumWeight.zero || other.equals(NumWeight.zero)) return NumWeight.zero
    val o = other.asInstanceOf[NumWeight]
    new NumWeight(o.i + i)
  }

  @Override def combineWith(other: Nothing): Nothing = {
    if (other.equals(NumWeight.zero)) return this
    if (this == NumWeight.zero) return other
    val o = other.asInstanceOf[NumWeight]
    if (o.i == i) return o
    NumWeight.zero
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
    val other = obj.asInstanceOf[NumWeight]
    if (i != other.i) return false
    true
  }
}