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

abstract class Rule[N <: Location, D <: State, W <: Weight](protected var s1: D, protected var l1: N, protected var s2: D, protected var l2: N, protected var w: W) {
  def getStartConfig = new Nothing(l1, s1)

  def getTargetConfig = new Nothing(l2, s2)

  def getL1: N = l1

  def getL2: N = l2

  def getS1: D = s1

  def getS2: D = s2

  def setS1(s1: D): Unit = {
    this.s1 = s1
  }

  def getWeight: W = w

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (l1 == null) 0
    else l1.hashCode)
    result = prime * result + (if (l2 == null) 0
    else l2.hashCode)
    result = prime * result + (if (s1 == null) 0
    else s1.hashCode)
    result = prime * result + (if (s2 == null) 0
    else s2.hashCode)
    result = prime * result + (if (w == null) 0
    else w.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Rule[_ <: Nothing, _ <: Nothing, _ <: Nothing]]
    if (l1 == null) if (other.l1 != null) return false
    else if (!l1.equals(other.l1)) return false
    if (l2 == null) if (other.l2 != null) return false
    else if (!l2.equals(other.l2)) return false
    if (s1 == null) if (other.s1 != null) return false
    else if (!s1.equals(other.s1)) return false
    if (s2 == null) if (other.s2 != null) return false
    else if (!s2.equals(other.s2)) return false
    if (w == null) if (other.w != null) return false
    else if (!w.equals(other.w)) return false
    true
  }
}