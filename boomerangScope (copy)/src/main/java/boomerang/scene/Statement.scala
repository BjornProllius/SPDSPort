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

import java.util
import wpds.interfaces.Empty
import wpds.interfaces.Location

object Statement {
  // Wrapper for stmt so we know the method
  private var epsilon: Statement = null

  def epsilon: Statement = {
    if (epsilon == null) epsilon = new Statement.EpsStatement
    epsilon
  }

  private class EpsStatement extends Statement("Eps_s") with Nothing {
    @Override override def getMethod: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def containsStaticFieldAccess: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def containsInvokeExpr: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getWrittenField: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isFieldWriteWithBase(base: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getLoadedField: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isFieldLoadWithBase(base: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isParameter(value: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def assignsValue(value: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isReturnOperator(`val`: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def uses(value: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isAssign: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getLeftOp: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def getRightOp: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isInstanceOfStatement(fact: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isCast: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getInvokeExpr: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isReturnStmt: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isThrowStmt: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isIfStmt: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getIfStmt: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def getReturnOp: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isMultiArrayAllocation: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isStringAllocation: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isFieldStore: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isArrayStore: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isArrayLoad: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isFieldLoad: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isIdentityStmt: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def killAtIfStmt(fact: Nothing, successor: Statement): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getFieldStore: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def getFieldLoad: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isStaticFieldLoad: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def isStaticFieldStore: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getStaticField: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def isPhiStatement: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def getPhiVals: Nothing = null

    @Override override def getArrayBase: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override override def getStartLineNumber: Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override override def getStartColumnNumber: Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override override def getEndColumnNumber: Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override override def getEndLineNumber: Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override override def isCatchStmt: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override override def hashCode: Int = System.identityHashCode(this)

    @Override override def equals(obj: Nothing): Boolean = obj eq this
  }
}

abstract class Statement extends Nothing {
  final private var rep: Nothing = null
  final protected var method: Nothing = null

  def this(method: Nothing) {
    this()
    this.rep = null
    this.method = method
  }

  def this(rep: Nothing) {
    this()
    this.rep = rep
    this.method = null
  }

  @Override def toString: Nothing = rep

  def getMethod: Nothing = this.method

  def containsStaticFieldAccess: Boolean

  def containsInvokeExpr: Boolean

  def getWrittenField: Nothing

  def isFieldWriteWithBase(base: Nothing): Boolean

  def getLoadedField: Nothing

  def isFieldLoadWithBase(base: Nothing): Boolean

  def isParameter(value: Nothing): Boolean = {
    if (containsInvokeExpr) {
      val invokeExpr = getInvokeExpr
      if (invokeExpr.isInstanceInvokeExpr) if (invokeExpr.getBase.equals(value)) return true
      import scala.collection.JavaConversions._
      for (arg <- invokeExpr.getArgs) {
        if (arg.equals(value)) return true
      }
    }
    false
  }

  def getParameter(value: Nothing): Int = {
    if (containsInvokeExpr) {
      val invokeExpr = getInvokeExpr
      if (invokeExpr.isInstanceInvokeExpr) if (invokeExpr.getBase.equals(value)) return -2
      var index = 0
      import scala.collection.JavaConversions._
      for (arg <- invokeExpr.getArgs) {
        if (arg.equals(value)) return index
        index += 1
      }
    }
    -1
  }

  def isReturnOperator(`val`: Nothing): Boolean = {
    if (isReturnStmt) return getReturnOp.equals(`val`)
    false
  }

  def uses(value: Nothing): Boolean = {
    if (value.isStatic) return true
    if (assignsValue(value)) return true
    if (isFieldStore) if (getFieldStore.getX.equals(value)) return true
    if (isReturnOperator(value)) return true
    if (isParameter(value)) return true
    false
  }

  def assignsValue(value: Nothing): Boolean = {
    if (isAssign) if (getLeftOp.equals(value)) return true
    false
  }

  def isAssign: Boolean

  def getLeftOp: Nothing

  def getRightOp: Nothing

  def isInstanceOfStatement(fact: Nothing): Boolean

  def isCast: Boolean

  def isPhiStatement: Boolean

  def getInvokeExpr: Nothing

  def isReturnStmt: Boolean

  def isThrowStmt: Boolean

  def isIfStmt: Boolean

  def getIfStmt: Nothing

  def getReturnOp: Nothing

  def isMultiArrayAllocation: Boolean

  def isStringAllocation: Boolean

  def isFieldStore: Boolean

  def isArrayStore: Boolean

  def isArrayLoad: Boolean

  def isFieldLoad: Boolean

  def isIdentityStmt: Boolean

  def getFieldStore: Nothing

  def getFieldLoad: Nothing

  def isStaticFieldLoad: Boolean

  def isStaticFieldStore: Boolean

  def getStaticField: Nothing

  /**
   * This method kills a data-flow at an if-stmt, it is assumed that the propagated "allocation"
   * site is x = null and fact is the propagated aliased variable. (i.e., y after a statement y =
   * x). If the if-stmt checks for if y != null or if y == null, data-flow propagation can be killed
   * when along the true/false branch.
   *
   * @param fact The data-flow value that bypasses the if-stmt
   * @return true if the Val fact shall be killed
   */
  def killAtIfStmt(fact: Nothing, successor: Statement): Boolean

  def getPhiVals: Nothing

  def getArrayBase: Nothing

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (method == null) 0
    else method.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Statement]
    if (method == null) if (other.method != null) return false
    else if (!method.equals(other.method)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    true
  }

  def getStartLineNumber: Int

  def getStartColumnNumber: Int

  def getEndLineNumber: Int

  def getEndColumnNumber: Int

  def isCatchStmt: Boolean

  @Override def accepts(other: Nothing): Boolean = this == other
}