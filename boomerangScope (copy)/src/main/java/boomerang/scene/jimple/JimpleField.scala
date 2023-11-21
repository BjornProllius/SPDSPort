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
package boomerang.scene.jimple

import boomerang.scene.Field
import soot.SootField

class JimpleField(private val delegate: Nothing) extends Nothing {
  @Override def toString: Nothing = delegate.getName.toString

  def getSootField: Nothing = this.delegate

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (delegate == null) 0
    else delegate.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[JimpleField]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def isInnerClassField: Boolean = this.delegate.getName.contains("$")
}