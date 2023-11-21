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

import pathexpression.Edge
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.wildcard.Wildcard

class Transition[N <: Location, D <: State](private val s1: D, private val l1: N, private val s2: D) extends Nothing {
  assert(s1 != null)
  assert(s2 != null)
  assert(l1 != null)
  if (l1.isInstanceOf[Nothing]) throw new Nothing("No wildcards allowed!")
  private var hashCode = 0

  def getStartConfig = new Nothing(l1, s1)

  def getTarget: D = s2

  def getStart: D = s1

  @Override def hashCode: Int = {
    if (hashCode != 0) return hashCode
    val prime = 31
    var result = 1
    result = prime * result + (if (l1 == null) 0
    else l1.hashCode)
    result = prime * result + (if (s1 == null) 0
    else s1.hashCode)
    result = prime * result + (if (s2 == null) 0
    else s2.hashCode)
    hashCode = result
    hashCode
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Transition[_ <: Nothing, _ <: Nothing]]
    if (l1 == null) if (other.l1 != null) return false
    else if (!l1.equals(other.l1)) return false
    if (s1 == null) if (other.s1 != null) return false
    else if (!s1.equals(other.s1)) return false
    if (s2 == null) if (other.s2 != null) return false
    else if (!s2.equals(other.s2)) return false
    true
  }

  @Override def toString: Nothing = s1 + "~" + l1 + "~>" + s2

  @Override def getLabel: N = l1
}