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
package inference

import boomerang.scene.Method
import java.util.Collections
import java.util
import wpds.impl.Weight

object InferenceWeight {
  private var one: InferenceWeight = null
  private var zero: InferenceWeight = null

  def one: InferenceWeight = {
    if (one == null) one = new InferenceWeight("ONE")
    one
  }

  def zero: InferenceWeight = {
    if (zero == null) zero = new InferenceWeight("ZERO")
    zero
  }
}

class InferenceWeight extends Nothing {
  final private var invokedMethods: Nothing = null
  final private var rep: Nothing = null

  def this(rep: Nothing) {
    this()
    this.rep = rep
    this.invokedMethods = null
  }

  def this(res: Nothing) {
    this()
    this.invokedMethods = res
    this.rep = null
  }

  def this(m: Nothing) {
    this()
    this.invokedMethods = Collections.singleton(m)
    this.rep = null
  }

  @Override def extendWith(other: Nothing): Nothing = {
    if (other.equals(InferenceWeight.one)) return this
    if (this == InferenceWeight.one) return other
    if (other.equals(InferenceWeight.zero) || this == InferenceWeight.zero) return InferenceWeight.zero
    val func = other.asInstanceOf[InferenceWeight]
    val otherInvokedMethods = func.invokedMethods
    val res = new Nothing(invokedMethods)
    res.addAll(otherInvokedMethods)
    new InferenceWeight(res)
  }

  @Override def combineWith(other: Nothing): Nothing = extendWith(other)

  def toString: Nothing = {
    if (this.rep != null) return this.rep
    "{Func:" + invokedMethods.toString + "}"
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result = prime * result + (if (invokedMethods == null) 0
    else invokedMethods.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[InferenceWeight]
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    if (invokedMethods == null) if (other.invokedMethods != null) return false
    else if (!invokedMethods.equals(other.invokedMethods)) return false
    true
  }
}