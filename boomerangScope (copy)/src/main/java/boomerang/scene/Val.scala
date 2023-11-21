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
package boomerang.scene

object Val {
  private var zeroInstance: Val = null

  def zero: Val = {
    if (zeroInstance == null) zeroInstance = new Val("ZERO") {
      @Override override def getType: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isStatic: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isNewExpr: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getNewExprType: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def asUnbalanced(stmt: Nothing): Val = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isLocal: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isArrayAllocationVal: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isNull: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isStringConstant: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getStringValue: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isStringBufferOrBuilder: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isThrowableAllocationType: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isCast: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getCastOp: Val = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isArrayRef: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isInstanceOfExpr: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getInstanceOfOp: Val = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isLengthExpr: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getLengthOp: Val = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isIntConstant: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isClassConstant: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getClassConstantType: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def withNewMethod(callee: Nothing): Val = null

      @Override override def isLongConstant: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getIntValue: Int = {
        // TODO Auto-generated method stub
        0
      }

      @Override override def getLongValue: Long = {
        // TODO Auto-generated method stub
        0
      }

      @Override override def getArrayBase: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def getVariableName: Nothing = toString
    }
    zeroInstance
  }
}

abstract class Val protected {
  this.rep = null
  this.m = null
  this.unbalancedStmt = null
  final protected var m: Nothing = null
  final private var rep: Nothing = null
  final protected var unbalancedStmt: Nothing = null

  def this(m: Nothing) {
    this()
    this.rep = null
    this.m = m
    this.unbalancedStmt = null
  }

  def this(m: Nothing, unbalancedStmt: Nothing) {
    this()
    this.rep = null
    this.m = m
    this.unbalancedStmt = unbalancedStmt
  }

  def this(rep: Nothing) {
    this()
    this.rep = rep
    this.m = null
    this.unbalancedStmt = null
  }

  def getType: Nothing

  def m: Nothing = m

  @Override def toString: Nothing = rep

  def isStatic: Boolean

  def isNewExpr: Boolean

  def getNewExprType: Nothing

  def isUnbalanced: Boolean = unbalancedStmt != null && rep == null

  def asUnbalanced(stmt: Nothing): Val

  def isLocal: Boolean

  def isArrayAllocationVal: Boolean

  def isNull: Boolean

  def isStringConstant: Boolean

  def getStringValue: Nothing

  def isStringBufferOrBuilder: Boolean

  def isThrowableAllocationType: Boolean

  def isCast: Boolean

  def getCastOp: Val

  def isArrayRef: Boolean

  def isInstanceOfExpr: Boolean

  def getInstanceOfOp: Val

  def isLengthExpr: Boolean

  def getLengthOp: Val

  def isIntConstant: Boolean

  def isClassConstant: Boolean

  def getClassConstantType: Nothing

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (m == null) 0
    else m.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result = prime * result + (if (unbalancedStmt == null) 0
    else unbalancedStmt.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Val]
    if (m == null) if (other.m != null) return false
    else if (!m.equals(other.m)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    if (unbalancedStmt == null) if (other.unbalancedStmt != null) return false
    else if (!unbalancedStmt.equals(other.unbalancedStmt)) return false
    true
  }

  def withNewMethod(callee: Nothing): Val

  def withSecondVal(leftOp: Val) = throw new Nothing("Unfinished")

  def isLongConstant: Boolean

  def isConstant: Boolean = isClassConstant || isIntConstant || isStringConstant || isLongConstant

  def getIntValue: Int

  def getLongValue: Long

  def getArrayBase: Nothing

  def isThisLocal: Boolean = if (m.isStatic) false
  else m.getThisLocal.equals(this)

  def isReturnLocal: Boolean = m.getReturnLocals.contains(this)

  def isParameterLocal(i: Int): Boolean = i < m.getParameterLocals.size && m.getParameterLocal(i).equals(this)

  def getVariableName: Nothing
}