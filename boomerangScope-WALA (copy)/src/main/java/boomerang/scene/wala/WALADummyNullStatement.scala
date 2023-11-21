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
import boomerang.scene.IfStatement
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Type
import boomerang.scene.Val
import com.ibm.wala.analysis.typeInference.TypeAbstraction
import java.util

class WALADummyNullStatement(private var leftOp: Nothing, method: Nothing) extends Nothing(a + " = null", method) {
  this.rightOp = new Nothing((-1, method.asInstanceOf[Nothing])) {
    @Override def isNull = true

    @Override def getType = new Nothing(TypeAbstraction.TOP)

    @Override def hashCode: Int = System.identityHashCode(this)

    @Override def equals(obj: Nothing): Boolean = this eq obj
  }
  private var rightOp: Nothing = null

  @Override def containsStaticFieldAccess = false

  @Override def containsInvokeExpr = false

  @Override def getWrittenField = throw new Nothing("Illegal")

  @Override def isFieldWriteWithBase(base: Nothing) = false

  @Override def getLoadedField = throw new Nothing("Illegal")

  @Override def isFieldLoadWithBase(base: Nothing) = false

  @Override def isAssign = true

  @Override def getLeftOp: Nothing = leftOp

  @Override def getRightOp: Nothing = rightOp

  @Override def isInstanceOfStatement(fact: Nothing) = false

  @Override def isCast = false

  @Override def isPhiStatement = false

  @Override def getInvokeExpr = throw new Nothing("Illegal")

  @Override def isReturnStmt = false

  @Override def isThrowStmt = false

  @Override def isIfStmt = false

  @Override def getIfStmt = throw new Nothing("Illegal")

  @Override def getReturnOp = throw new Nothing("Illegal")

  @Override def isMultiArrayAllocation = false

  @Override def isStringAllocation = false

  @Override def isFieldStore = false

  @Override def isArrayStore = false

  @Override def isArrayLoad = false

  @Override def isFieldLoad = false

  @Override def isIdentityStmt = false

  @Override def getFieldStore = throw new Nothing("Illegal")

  @Override def getFieldLoad = throw new Nothing("Illegal")

  @Override def isStaticFieldLoad = false

  @Override def isStaticFieldStore = false

  @Override def getStaticField = throw new Nothing("Illegal")

  @Override def killAtIfStmt(fact: Nothing, successor: Nothing) = false

  @Override def getPhiVals = throw new Nothing("Illegal")

  @Override def getArrayBase = throw new Nothing("Illegal")

  @Override def getStartLineNumber = 0

  @Override def isCatchStmt = false

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (leftOp == null) 0
    else leftOp.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[WALADummyNullStatement]
    if (leftOp == null) if (other.leftOp != null) return false
    else if (!leftOp.equals(other.leftOp)) return false
    true
  }

  @Override def toString: Nothing = leftOp + " = null"
}