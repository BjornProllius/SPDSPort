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

import boomerang.scene.AllocVal
import boomerang.scene.Type
import boomerang.scene.Val
import boomerang.scene.WrappedClass
import com.ibm.wala.analysis.typeInference.ConeType
import com.ibm.wala.analysis.typeInference.PointType
import com.ibm.wala.analysis.typeInference.TypeAbstraction
import com.ibm.wala.classLoader.IClass
import com.ibm.wala.types.TypeReference
import java.util

class WALAType(private val typeAbstraction: Nothing) extends Nothing {
  @Override def isNullType: Boolean = typeAbstraction.equals(TypeAbstraction.TOP)

  @Override def isRefType = true

  @Override def isBooleanType: Boolean = {
    // TODO
    false
  }

  @Override def isArrayType: Boolean = typeAbstraction.getTypeReference != null && typeAbstraction.getTypeReference.isArrayType

  @Override def getArrayBaseType: Nothing = null

  @Override def getWrappedClass: Nothing = {
    // TODO Auto-generated method stub
    new Nothing(typeAbstraction.getTypeReference)
  }

  @Override def doesCastFail(targetVal: Nothing, target: Nothing): Boolean = {
    if (target.isInstanceOf[Nothing] && target.asInstanceOf[Nothing].getAllocVal.isNewExpr) {
      val subclassOfNew = this.isSubclassOf(targetVal.asInstanceOf[WALAType])
      return !subclassOfNew
    }
    val castFails = this.isSubclassOf(targetVal.asInstanceOf[WALAType]) || targetVal.asInstanceOf[WALAType].isSubclassOf(this)
    !castFails
  }

  private def isSubclassOf(targetType: WALAType) = {
    val meet = typeAbstraction.meet(targetType.typeAbstraction)
    meet.equals(typeAbstraction)
  }

  private def getDelegate = typeAbstraction

  @Override def isSubtypeOf(`type`: Nothing): Boolean = {
    val newType = "L" + `type`.replace(".", "/")
    if (typeAbstraction.isInstanceOf[Nothing]) {
      val coneType = typeAbstraction.asInstanceOf[Nothing]
      // TODO misplaced
      if (coneType.isArrayType) return true
      if (hasClassInterface(coneType.getType, newType)) return true
      val iterateImplementors = coneType.iterateImplementors
      while (iterateImplementors.hasNext) {
        val next = iterateImplementors.next
        if (hasClassInterface(next, newType)) return true
      }
    }
    if (typeAbstraction.isInstanceOf[Nothing]) {
      val pointType = typeAbstraction.asInstanceOf[Nothing]
      // TODO misplaced
      if (pointType.isArrayType) return true
      if (hasClassInterface(pointType.getIClass, newType)) return true
    }
    if (typeAbstraction.equals(TypeAbstraction.TOP)) return true
    false
  }

  private def hasClassInterface(next: Nothing, t: Nothing): Boolean = {
    if (next.getName.toString.equals(t)) return true
    import scala.collection.JavaConversions._
    for (i <- next.getAllImplementedInterfaces) {
      if (i.getName.toString.equals(t)) return true
    }
    false
  }

  @Override def toString: Nothing = {
    if (typeAbstraction.equals(TypeAbstraction.TOP)) return "TOP"
    if (typeAbstraction.isInstanceOf[Nothing]) {
      val pointType = typeAbstraction.asInstanceOf[Nothing]
      return toString(pointType.getTypeReference)
    }
    if (typeAbstraction.isInstanceOf[Nothing]) {
      val coneType = typeAbstraction.asInstanceOf[Nothing]
      return toString(coneType.getTypeReference)
    }
    typeAbstraction.toString
  }

  private def toString(typeReference: Nothing): Nothing = {
    if (typeReference.isPrimitiveType) {
      if (typeReference eq TypeReference.Byte) return "byte"
      if (typeReference eq TypeReference.Char) return "char"
      if (typeReference eq TypeReference.Boolean) return "boolean"
      if (typeReference eq TypeReference.Int) return "int"
      if (typeReference eq TypeReference.Long) return "long"
      if (typeReference eq TypeReference.Short) return "short"
    }
    if (typeReference.isArrayType) return toString(typeReference.getArrayElementType) + "[]"
    typeReference.getName.toString
  }
}