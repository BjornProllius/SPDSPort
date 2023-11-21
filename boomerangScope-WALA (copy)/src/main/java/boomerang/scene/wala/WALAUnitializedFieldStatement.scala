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
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import java.util

class WALAUnitializedFieldStatement(private var field: Nothing, private var method: Nothing, private var thisLocal: Nothing, private var rightOp: Nothing) extends Nothing("this." + field + " = " + rightOp, method) {
  @Override def containsStaticFieldAccess = false

  @Override def containsInvokeExpr = false

  @Override def getWrittenField: Nothing = field

  @Override def getRightOp: Nothing = rightOp

  @Override def isFieldWriteWithBase(base: Nothing): Boolean = thisLocal.equals(base)

  @Override def getLoadedField = throw new Nothing("Illegal")

  @Override def isFieldLoadWithBase(base: Nothing) = false

  @Override def isAssign = true

  @Override def isInstanceOfStatement(fact: Nothing) = false

  @Override def isCast = false

  @Override def isPhiStatement = false

  @Override def getInvokeExpr: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def isReturnStmt = false

  @Override def isThrowStmt = false

  @Override def isIfStmt = false

  @Override def getIfStmt = throw new Nothing("Illegal")

  @Override def getReturnOp = throw new Nothing("Illegal")

  @Override def isMultiArrayAllocation = false

  @Override def isStringAllocation = false

  @Override def isFieldStore = true

  @Override def isArrayStore = false

  @Override def isArrayLoad = false

  @Override def isFieldLoad = false

  @Override def isIdentityStmt = false

  @Override def getFieldStore = new Nothing(thisLocal, field)

  @Override def getFieldLoad = throw new Nothing("Illegal")

  @Override def isStaticFieldLoad = false

  @Override def isStaticFieldStore = false

  @Override def getStaticField = throw new Nothing("Illegal")

  @Override def killAtIfStmt(fact: Nothing, successor: Nothing) = false

  @Override def getPhiVals: Nothing = null

  @Override def getArrayBase = throw new Nothing("Illegal")

  @Override def getStartLineNumber = 0

  @Override def isCatchStmt = false
}