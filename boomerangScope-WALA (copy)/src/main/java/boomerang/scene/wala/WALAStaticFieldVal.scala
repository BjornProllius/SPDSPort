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

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.StaticFieldVal
import boomerang.scene.Type
import boomerang.scene.Val

class WALAStaticFieldVal(private var declaredField: Nothing, method: Nothing, unbalanced: Nothing) extends Nothing(method) {
  def this(declaredField: Nothing, method: Nothing) {
    this(declaredField, method, null)
  }

  @Override def field: Nothing = declaredField

  @Override def asUnbalanced(stmt: Nothing) = new WALAStaticFieldVal(declaredField, m, stmt)

  @Override def getType = throw new Nothing("Fault!")

  @Override def isStatic = true

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

  @Override def withNewMethod(callee: Nothing) = new WALAStaticFieldVal(declaredField, callee, unbalancedStmt)

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (declaredField == null) 0
    else declaredField.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[WALAStaticFieldVal]
    if (declaredField == null) if (other.declaredField != null) return false
    else if (!declaredField.equals(other.declaredField)) return false
    true
  }

  @Override def isLongConstant: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getIntValue: Int = {
    // TODO Auto-generated method stub
    0
  }

  @Override def getLongValue: Long = {
    // TODO Auto-generated method stub
    0
  }

  @Override def getArrayBase: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def getVariableName: Nothing = toString
}