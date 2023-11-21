/**
 * ***************************************************************************** Copyright (c) 2020
 * CodeShield GmbH, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang.scene.wala

import boomerang.scene.Field
import com.ibm.wala.types.FieldReference

class WALAField(private var fieldRef: Nothing) extends Nothing {
  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (fieldRef == null) 0
    else fieldRef.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[WALAField]
    if (fieldRef == null) if (other.fieldRef != null) return false
    else if (!fieldRef.equals(other.fieldRef)) return false
    true
  }

  @Override def toString: Nothing = fieldRef.toString

  @Override def isInnerClassField = throw new Nothing("Not yet implemented")
}