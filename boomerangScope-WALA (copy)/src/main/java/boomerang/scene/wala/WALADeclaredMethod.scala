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

import boomerang.scene.DeclaredMethod
import boomerang.scene.InvokeExpr
import boomerang.scene.WrappedClass
import com.ibm.wala.types.MethodReference

class WALADeclaredMethod(inv: Nothing, private val delegate: Nothing) extends Nothing(inv) {
  this.isStatic = inv.isStaticInvokeExpr
  final private var isStatic = false

  @Override def isNative: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getSubSignature: Nothing = delegate.getName.toString

  @Override def getName: Nothing = delegate.getName.toString

  @Override def isStatic: Boolean = isStatic

  @Override def isConstructor: Boolean = delegate.isInit

  @Override def getSignature: Nothing = delegate.getSignature

  @Override def getDeclaringClass = new Nothing(delegate.getDeclaringClass)

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (delegate == null) 0
    else delegate.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[WALADeclaredMethod]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def toString: Nothing = delegate.toString
}