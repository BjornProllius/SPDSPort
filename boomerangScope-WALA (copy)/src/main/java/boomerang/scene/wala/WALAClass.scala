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

import boomerang.scene.Method
import boomerang.scene.Type
import boomerang.scene.WrappedClass
import com.ibm.wala.cast.java.ipa.callgraph.JavaSourceAnalysisScope
import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.types.TypeReference
import java.util

class WALAClass(private val delegate: Nothing) extends Nothing {
  @Override def getMethods: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def hasSuperclass: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getSuperclass: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def getType: Nothing = null

  @Override def isApplicationClass: Boolean = delegate.getClassLoader.equals(ClassLoaderReference.Application) || delegate.getClassLoader.equals(JavaSourceAnalysisScope.SOURCE)

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
    val other = obj.asInstanceOf[WALAClass]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def getFullyQualifiedName: Nothing = {
    if (delegate.getName == null) return "FAILED"
    if (delegate.getName.getPackage == null) return getName
    delegate.getName.getPackage.toString.replace("/", ".") + "." + delegate.getName.getClassName
  }

  @Override def getName: Nothing = delegate.getName.getClassName.toString

  @Override def getDelegate: Nothing = delegate
}