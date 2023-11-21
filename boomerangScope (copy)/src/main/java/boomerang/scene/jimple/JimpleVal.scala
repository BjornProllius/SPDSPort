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

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.StaticFieldVal
import boomerang.scene.Type
import boomerang.scene.Val
import soot.Local
import soot.NullType
import soot.Scene
import soot.Value
import soot.jimple.ArrayRef
import soot.jimple.CastExpr
import soot.jimple.ClassConstant
import soot.jimple.InstanceOfExpr
import soot.jimple.IntConstant
import soot.jimple.LengthExpr
import soot.jimple.LongConstant
import soot.jimple.NewArrayExpr
import soot.jimple.NewExpr
import soot.jimple.NewMultiArrayExpr
import soot.jimple.NullConstant
import soot.jimple.StaticFieldRef
import soot.jimple.StringConstant

class JimpleVal protected(private val v: Nothing, m: Nothing, unbalanced: Nothing) extends Nothing(m, unbalanced) {
  if (v == null) throw new Nothing("Value must not be null!")

  def this(v: Nothing, m: Nothing) {
    this(v, m, null)
  }

  def getType: Nothing = if (v == null) new Nothing(NullType.v)
  else new Nothing(v.getType)

  def m: Nothing = m

  @Override def toString: Nothing = v.toString + " (" + m.getDeclaringClass + "." + m + ")" + (if (isUnbalanced) " unbalanaced " + unbalancedStmt
  else "")

  def isStatic = false

  def isNewExpr: Boolean = v.isInstanceOf[Nothing]

  def getNewExprType = new Nothing(v.asInstanceOf[Nothing].getType)

  def asUnbalanced(stmt: Nothing) = new JimpleVal(v, m, stmt)

  def isLocal: Boolean = v.isInstanceOf[Nothing]

  def isArrayAllocationVal: Boolean = {
    if (v.isInstanceOf[Nothing]) {
      val expr = v.asInstanceOf[Nothing]
      // TODO Performance issue?!
      //            return expr.getBaseType() instanceof RefType;
      return true
    }
    else if (v.isInstanceOf[Nothing]) return true
    false
  }

  def isNull: Boolean = v.isInstanceOf[Nothing]

  def isStringConstant: Boolean = v.isInstanceOf[Nothing]

  def getStringValue: Nothing = v.asInstanceOf[Nothing].value

  def isStringBufferOrBuilder: Boolean = {
    val `type` = getType
    `type`.toString.equals("java.lang.String") || `type`.toString.equals("java.lang.StringBuilder") || `type`.toString.equals("java.lang.StringBuffer")
  }

  def isThrowableAllocationType: Boolean = Scene.v.getOrMakeFastHierarchy.canStoreType(getType.getDelegate, Scene.v.getType("java.lang.Throwable"))

  def isCast: Boolean = v.isInstanceOf[Nothing]

  def getCastOp: Nothing = {
    val cast = v.asInstanceOf[Nothing]
    new JimpleVal(cast.getOp, m)
  }

  def isInstanceFieldRef: Boolean = v.isInstanceOf[Nothing]

  def isStaticFieldRef: Boolean = v.isInstanceOf[Nothing]

  def getStaticField: Nothing = {
    val `val` = v.asInstanceOf[Nothing]
    new Nothing(new Nothing(`val`.getField), m)
  }

  def isArrayRef: Boolean = v.isInstanceOf[Nothing]

  @Override def getArrayBase = new Nothing(new JimpleVal(v.asInstanceOf[Nothing].getBase, m), if (v.asInstanceOf[Nothing].getIndex.isInstanceOf[Nothing]) (v.asInstanceOf[Nothing]).getIndex.asInstanceOf[Nothing].value
  else -1)

  def isInstanceOfExpr: Boolean = v.isInstanceOf[Nothing]

  def getInstanceOfOp: Nothing = {
    val `val` = v.asInstanceOf[Nothing]
    new JimpleVal(`val`.getOp, m)
  }

  def isLengthExpr: Boolean = v.isInstanceOf[Nothing]

  def getLengthOp: Nothing = {
    val `val` = v.asInstanceOf[Nothing]
    new JimpleVal(`val`.getOp, m)
  }

  def isIntConstant: Boolean = v.isInstanceOf[Nothing]

  def getIntValue: Int = v.asInstanceOf[Nothing].value

  def isLongConstant: Boolean = v.isInstanceOf[Nothing]

  def getLongValue: Long = v.asInstanceOf[Nothing].value

  def isClassConstant: Boolean = v.isInstanceOf[Nothing]

  def getClassConstantType = new Nothing(v.asInstanceOf[Nothing].toSootType)

  @Override def withNewMethod(callee: Nothing) = throw new Nothing("Only allowed for static fields")

  @Override def withSecondVal(leftOp: Nothing) = new Nothing(v, m, leftOp)

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (v == null) 0
    else v.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[JimpleVal]
    if (v == null) if (other.v != null) return false
    else if (!v.equals(other.v)) return false
    true
  }

  def getDelegate: Nothing = v

  @Override def getVariableName: Nothing = v.toString
}