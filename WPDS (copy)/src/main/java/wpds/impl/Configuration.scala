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

class Configuration[N <: Location, D <: State](private var location: N, private var state: D) {
  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (location == null) 0
    else location.hashCode)
    result = prime * result + (if (state == null) 0
    else state.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Configuration[_ <: Nothing, _ <: Nothing]]
    if (location == null) if (other.location != null) return false
    else if (!location.equals(other.location)) return false
    if (state == null) if (other.state != null) return false
    else if (!state.equals(other.state)) return false
    true
  }
}