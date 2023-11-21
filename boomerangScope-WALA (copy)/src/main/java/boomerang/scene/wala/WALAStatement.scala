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
import boomerang.scene.Val
import boomerang.scene.wala.WALAVal.OP
import com.google.common.collect.Lists
import com.ibm.wala.cast.loader.AstMethod
import com.ibm.wala.classLoader.IBytecodeMethod
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.shrikeCT.InvalidClassFileException
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSAArrayLoadInstruction
import com.ibm.wala.ssa.SSAArrayReferenceInstruction
import com.ibm.wala.ssa.SSAArrayStoreInstruction
import com.ibm.wala.ssa.SSACheckCastInstruction
import com.ibm.wala.ssa.SSAConditionalBranchInstruction
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ssa.SSAGetInstruction
import com.ibm.wala.ssa.SSAInstanceofInstruction
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ssa.SSANewInstruction
import com.ibm.wala.ssa.SSAPhiInstruction
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.ssa.SSAReturnInstruction
import com.ibm.wala.ssa.SSAThrowInstruction
import java.util

class WALAStatement extends Nothing {
  final private var delegate: Nothing = null
  final private var rep: Nothing = null
  private var phiUseCache: Nothing = null

  def this(stmt: Nothing, method: Nothing) {
    this()
    super (method)
    this.delegate = stmt
    this.rep = null
  }

  def this(string: Nothing, method: Nothing) {
    this()
    super (method)
    this.rep = string
    this.delegate = null
  }

  @Override def containsStaticFieldAccess: Boolean = isStaticFieldLoad || isStaticFieldStore

  @Override def containsInvokeExpr: Boolean = delegate.isInstanceOf[Nothing]

  @Override def getWrittenField: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    new Nothing(ins.getDeclaredField)
  }

  @Override def isFieldWriteWithBase(base: Nothing): Boolean = {
    if (delegate.isInstanceOf[Nothing]) {
      val ins = delegate.asInstanceOf[Nothing]
      return base.equals(new Nothing(ins.getRef, method.asInstanceOf[Nothing]))
    }
    false
  }

  @Override def getLoadedField: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    new Nothing(ins.getDeclaredField)
  }

  @Override def isFieldLoadWithBase(base: Nothing): Boolean = {
    if (delegate.isInstanceOf[Nothing]) {
      val ins = delegate.asInstanceOf[Nothing]
      return base.equals(new Nothing(ins.getRef, method.asInstanceOf[Nothing]))
    }
    false
  }

  @Override def isReturnOperator(`val`: Nothing): Boolean = {
    if (isReturnStmt) {
      val ins = delegate.asInstanceOf[Nothing]
      return if (ins.getResult eq -(1)) false
      else new Nothing(ins.getResult, method.asInstanceOf[Nothing]).equals(`val`)
    }
    false
  }

  def isPhiStatement: Boolean = delegate.isInstanceOf[Nothing]

  def getPhiVals: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    if (phiUseCache == null) {
      phiUseCache = Lists.newArrayList
      var i = 0
      while (i < ins.getNumberOfUses) {
        phiUseCache.add(new Nothing(ins.getUse(i), method.asInstanceOf[Nothing]))
        i += 1
      }
    }
    phiUseCache
  }

  @Override def isAssign: Boolean = isFieldLoad || isFieldStore || isAllocationStatement || isPhiStatement || isAssigningCall || isCast || isArrayLoad || isArrayStore || isStaticFieldLoad || isStaticFieldStore

  private def isAssigningCall: Boolean = {
    if (containsInvokeExpr) if (delegate.asInstanceOf[Nothing].getNumberOfReturnValues > 0) return true
    false
  }

  private def isAllocationStatement = delegate.isInstanceOf[Nothing]

  @Override def getLeftOp: Nothing = {
    if (isFieldLoad || isStaticFieldLoad) return new Nothing(delegate.asInstanceOf[Nothing].getDef, method.asInstanceOf[Nothing])
    if (isFieldStore) {
      // The left op of a statement x.f = y must be the complete term x.f
      return new Nothing(delegate, OP.LEFT, method.asInstanceOf[Nothing])
    }
    if (isStaticFieldStore) return new Nothing(new Nothing(delegate.asInstanceOf[Nothing].getDeclaredField), method.asInstanceOf[Nothing])
    if (isAllocationStatement) return new Nothing(delegate.asInstanceOf[Nothing].getDef, method.asInstanceOf[Nothing])
    if (isPhiStatement) return new Nothing(delegate.asInstanceOf[Nothing].getDef, method.asInstanceOf[Nothing])
    if (isAssigningCall) return new Nothing(delegate.asInstanceOf[Nothing].getReturnValue(0), method.asInstanceOf[Nothing])
    if (isCast) return new Nothing(delegate.asInstanceOf[Nothing].getDef, method.asInstanceOf[Nothing])
    if (isArrayLoad) return new Nothing(delegate.asInstanceOf[Nothing].getDef, method.asInstanceOf[Nothing])
    if (isArrayStore) return new Nothing(delegate, OP.LEFT, method.asInstanceOf[Nothing])
    // TODO Auto-generated method stub
    null
  }

  @Override def getRightOp: Nothing = {
    if (isFieldLoad) {
      // The right op of a statement x = y.f must be the complete term y.f
      return new Nothing(delegate, OP.RIGHT, method.asInstanceOf[Nothing])
    }
    if (isFieldStore || isStaticFieldStore) return new Nothing(delegate.asInstanceOf[Nothing].getVal, method.asInstanceOf[Nothing])
    if (isAllocationStatement) return new Nothing(delegate.asInstanceOf[Nothing].getNewSite, method.asInstanceOf[Nothing])
    if (isPhiStatement || isAssigningCall) return new Nothing(delegate, OP.RIGHT, method.asInstanceOf[Nothing])
    if (isCast) return new Nothing(delegate.asInstanceOf[Nothing].getVal, method.asInstanceOf[Nothing])
    if (isArrayLoad) return new Nothing(delegate, OP.RIGHT, method.asInstanceOf[Nothing])
    if (isArrayStore) return new Nothing(delegate.asInstanceOf[Nothing].getValue, method.asInstanceOf[Nothing])
    if (isStaticFieldLoad) return new Nothing(new Nothing(delegate.asInstanceOf[Nothing].getDeclaredField), method.asInstanceOf[Nothing])
    null
  }

  @Override def isInstanceOfStatement(fact: Nothing): Boolean = delegate.isInstanceOf[Nothing]

  @Override def isCast: Boolean = delegate.isInstanceOf[Nothing]

  @Override def getInvokeExpr: Nothing = {
    val inv = delegate.asInstanceOf[Nothing]
    new Nothing(inv, method.asInstanceOf[Nothing])
  }

  @Override def isReturnStmt: Boolean = {
    if (delegate.isInstanceOf[Nothing]) {
      val ret = delegate.asInstanceOf[Nothing]
      return ret.getResult ne -1
    }
    false
  }

  @Override def getReturnOp: Nothing = {
    val ret = delegate.asInstanceOf[Nothing]
    new Nothing(ret.getResult, method.asInstanceOf[Nothing])
  }

  @Override def isThrowStmt: Boolean = delegate.isInstanceOf[Nothing]

  @Override def isIfStmt: Boolean = delegate.isInstanceOf[Nothing]

  @Override def getIfStmt = new Nothing(delegate.asInstanceOf[Nothing], method.asInstanceOf[Nothing])

  @Override def isMultiArrayAllocation: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def isStringAllocation: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def isFieldStore: Boolean = delegate.isInstanceOf[Nothing] && !(delegate.asInstanceOf[Nothing]).isStatic

  @Override def isArrayStore: Boolean = delegate.isInstanceOf[Nothing]

  @Override def isArrayLoad: Boolean = delegate.isInstanceOf[Nothing]

  @Override def isFieldLoad: Boolean = delegate.isInstanceOf[Nothing] && !(delegate.asInstanceOf[Nothing]).isStatic

  @Override def isIdentityStmt: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def killAtIfStmt(fact: Nothing, successor: Nothing): Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def toString: Nothing = if (delegate == null) rep
  else delegate.iIndex + ":" + delegate + " in " + method

  @Override def getFieldStore: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    new Nothing(new Nothing(ins.getRef, method.asInstanceOf[Nothing]), new Nothing(ins.getDeclaredField))
  }

  @Override def getFieldLoad: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    new Nothing(new Nothing(ins.getRef, method.asInstanceOf[Nothing]), new Nothing(ins.getDeclaredField))
  }

  @Override def isStaticFieldLoad: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].isStatic

  @Override def isStaticFieldStore: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].isStatic

  @Override def getStaticField: Nothing = {
    val stmt = delegate.asInstanceOf[Nothing]
    if (!stmt.isStatic) throw new Nothing("Not a static field access statement")
    new Nothing(new Nothing(stmt.getDeclaredField), method)
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    if (delegate.isInstanceOf[Nothing]) result = prime * result + delegate.asInstanceOf[Nothing].getDef
    result = prime * result + (if (delegate == null) 0
    else delegate.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    if (!super.equals(obj)) return false
    val other = obj.asInstanceOf[WALAStatement]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    if (delegate.isInstanceOf[Nothing] && other.delegate.isInstanceOf[Nothing]) if (!phiEquals(delegate.asInstanceOf[Nothing], other.delegate.asInstanceOf[Nothing])) return false
    true
  }

  /**
   * WALA computes ins1.equals(ins2) == true for two different phi instructions ins1 and ins2.
   *
   * @param phi1
   * @param phi2
   * @return
   */
  private def phiEquals(phi1: Nothing, phi2: Nothing) = phi1.getDef eq phi2.getDef

  @Override def getArrayBase: Nothing = {
    if (delegate.isInstanceOf[Nothing]) {
      val arrayRefIns = delegate.asInstanceOf[Nothing]
      return new Nothing(new Nothing(arrayRefIns.getArrayRef, method.asInstanceOf[Nothing]), arrayRefIns.getIndex)
    }
    throw new Nothing("Dead code")
  }

  @Override def getStartLineNumber: Int = {
    val m = method.asInstanceOf[Nothing].getIR.getMethod
    if (m.isInstanceOf[Nothing]) {
      val c = m.asInstanceOf[Nothing]
      return c.getLineNumber(delegate.iIndex)
    }
    val method = m.asInstanceOf[Nothing]
    var bytecodeIndex = 0
    try {
      bytecodeIndex = method.getBytecodeIndex(delegate.iIndex)
      return method.getLineNumber(bytecodeIndex)
    } catch {
      case e: Nothing =>

        // TODO Auto-generated catch block
        e.printStackTrace
    }
    -1
  }

  @Override def getStartColumnNumber: Int = -1

  @Override def getEndColumnNumber: Int = -1

  @Override def getEndLineNumber: Int = -1

  @Override def isCatchStmt = false

  def getSSAInstruction: Nothing = delegate
}