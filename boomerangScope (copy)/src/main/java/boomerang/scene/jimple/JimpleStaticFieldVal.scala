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
import boomerang.scene.Field
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.StaticFieldVal
import boomerang.scene.Type
import boomerang.scene.Val

class JimpleStaticFieldVal private(private val field: Nothing, m: Nothing, unbalanced: Nothing) extends Nothing(m, unbalanced) {
  def this(field: Nothing, m: Nothing) {
    this(field, m, null)
  }

  @Override def isStatic = true

  def toString: Nothing = "StaticField: " + field + m

  def field: Nothing = field

  @Override def asUnbalanced(stmt: Nothing) = new JimpleStaticFieldVal(field, m, stmt)

  @Override def getType = new Nothing(field.getSootField.getType)

  @Override def isNewExpr = false

  @Override def getNewExprType = throw new Nothing("Fault!")

  @Override def isLocal = false

  @Override def isArrayAllocationVal = false

  @Override def isNull = false

  @Override def isStringConstant = false

  @Override def getStringValue = throw new Nothing("Fault!")

  @Override def isStringBufferOrBuilder = false

  @Override def isThrowableAllocationType = false

  @Override def isCast = false

  @Override def getCastOp = throw new Nothing("Fault!")

  @Override def isArrayRef = false

  @Override def isInstanceOfExpr = false

  @Override def getInstanceOfOp = throw new Nothing("Fault!")

  @Override def isLengthExpr = false

  @Override def getLengthOp = throw new Nothing("Fault!")

  @Override def isIntConstant = false

  @Override def isClassConstant = false

  @Override def getClassConstantType = throw new Nothing("Fault!")

  @Override def withNewMethod(callee: Nothing) = new JimpleStaticFieldVal(field, callee)

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (field == null) 0
    else field.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[JimpleStaticFieldVal]
    if (field == null) if (other.field != null) return false
    else if (!field.equals(other.field)) return false
    true
  }

  @Override def isLongConstant = false

  @Override def getIntValue: Int = -1

  @Override def getLongValue: Long = -1

  @Override def getArrayBase: Nothing = null

  @Override def getVariableName: Nothing = toString
}