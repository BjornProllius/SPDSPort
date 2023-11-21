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
package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State

class PushRule[N <: Location, D <: State, W <: Weight](s1: D, l1: N, s2: D, l2: N, protected var callSite: N, w: W) extends Nothing(s1, l1, s2, l2, w) {
  def getCallSite: N = callSite

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (callSite == null) 0
    else callSite.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[PushRule[_ <: Nothing, _ <: Nothing, _ <: Nothing]]
    if (callSite == null) if (other.callSite != null) return false
    else if (!callSite.equals(other.callSite)) return false
    true
  }

  @Override def toString: Nothing = "<" + s1 + ";" + l1 + ">-><" + s2 + ";" + l2 + "." + callSite + ">(" + w + ")"
}