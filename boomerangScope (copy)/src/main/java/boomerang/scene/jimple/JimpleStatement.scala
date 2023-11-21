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

import boomerang.scene.Field
import boomerang.scene.IfStatement
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import com.google.common.base.Joiner
import java.util
import soot.jimple.ArrayRef
import soot.jimple.AssignStmt
import soot.jimple.CastExpr
import soot.jimple.CaughtExceptionRef
import soot.jimple.IdentityStmt
import soot.jimple.IfStmt
import soot.jimple.InstanceFieldRef
import soot.jimple.InstanceInvokeExpr
import soot.jimple.NewExpr
import soot.jimple.NewMultiArrayExpr
import soot.jimple.ReturnStmt
import soot.jimple.StaticFieldRef
import soot.jimple.Stmt
import soot.jimple.StringConstant
import soot.jimple.ThrowStmt
import soot.tagkit.SourceLnPosTag

object JimpleStatement {
  def create(delegate: Nothing, m: Nothing): Nothing = {
    val jimpleStatement = new JimpleStatement(delegate, m)
    jimpleStatement
  }
}

class JimpleStatement private(
                               // Wrapper for stmt so we know the method
                               private val delegate: Nothing, private val method: Nothing) extends Nothing(m) {
  if (delegate == null) throw new Nothing("Invalid, parameter may not be null")

  @Override def toString: Nothing = shortName(delegate)

  private def shortName(s: Nothing): Nothing = {
    if (s.containsInvokeExpr) {
      var base = ""
      if (s.getInvokeExpr.isInstanceOf[Nothing]) {
        val iie = s.getInvokeExpr.asInstanceOf[Nothing]
        base = iie.getBase.toString + "."
      }
      var assign = ""
      if (s.isInstanceOf[Nothing]) assign = s.asInstanceOf[Nothing].getLeftOp + " = "
      return assign + base + s.getInvokeExpr.getMethod.getName + "(" + Joiner.on(",").join(s.getInvokeExpr.getArgs) + ")"
    }
    if (s.isInstanceOf[Nothing]) return s.toString
    if (s.isInstanceOf[Nothing]) {
      val assignStmt = s.asInstanceOf[Nothing]
      if (assignStmt.getLeftOp.isInstanceOf[Nothing]) {
        val ifr = assignStmt.getLeftOp.asInstanceOf[Nothing]
        return ifr.getBase + "." + ifr.getField.getName + " = " + assignStmt.getRightOp
      }
      if (assignStmt.getRightOp.isInstanceOf[Nothing]) {
        val ifr = assignStmt.getRightOp.asInstanceOf[Nothing]
        return assignStmt.getLeftOp + " = " + ifr.getBase + "." + ifr.getField.getName
      }
      if (assignStmt.getRightOp.isInstanceOf[Nothing]) {
        val newExpr = assignStmt.getRightOp.asInstanceOf[Nothing]
        return assignStmt.getLeftOp + " = new " + newExpr.getBaseType.getSootClass.getShortName
      }
    }
    s.toString
  }

  def containsStaticFieldAccess: Boolean = {
    if (delegate.isInstanceOf[Nothing]) {
      val assignStmt = delegate.asInstanceOf[Nothing]
      return assignStmt.getLeftOp.isInstanceOf[Nothing] || assignStmt.getRightOp.isInstanceOf[Nothing]
    }
    false
  }

  def containsInvokeExpr: Boolean = delegate.containsInvokeExpr

  def getWrittenField: Nothing = {
    val as = delegate.asInstanceOf[Nothing]
    if (as.getLeftOp.isInstanceOf[Nothing]) {
      val staticFieldRef = as.getLeftOp.asInstanceOf[Nothing]
      return new Nothing(staticFieldRef.getField)
    }
    if (as.getLeftOp.isInstanceOf[Nothing]) return Field.array(getArrayBase.getY)
    val ifr = as.getLeftOp.asInstanceOf[Nothing]
    new Nothing(ifr.getField)
  }

  def isFieldWriteWithBase(base: Nothing): Boolean = {
    if (isAssign && isFieldStore) {
      val instanceFieldRef = getFieldStore
      return instanceFieldRef.getX.equals(base)
    }
    if (isAssign && isArrayStore) {
      val arrayBase = getArrayBase
      return arrayBase.getX.equals(base)
    }
    false
  }

  def getLoadedField: Nothing = {
    val as = delegate.asInstanceOf[Nothing]
    val ifr = as.getRightOp.asInstanceOf[Nothing]
    new Nothing(ifr.getField)
  }

  def isFieldLoadWithBase(base: Nothing): Boolean = {
    if (isAssign && isFieldLoad) return getFieldLoad.getX.equals(base)
    false
  }

  @Override def isAssign: Boolean = delegate.isInstanceOf[Nothing]

  def getLeftOp: Nothing = {
    assert(isAssign)
    val assignStmt = delegate.asInstanceOf[Nothing]
    new Nothing(assignStmt.getLeftOp, method)
  }

  def getRightOp: Nothing = {
    assert(isAssign)
    val assignStmt = delegate.asInstanceOf[Nothing]
    new Nothing(assignStmt.getRightOp, method)
  }

  def isInstanceOfStatement(fact: Nothing): Boolean = {
    if (isAssign) if (getRightOp.isInstanceOfExpr) {
      val instanceOfOp = getRightOp.getInstanceOfOp
      return instanceOfOp.equals(fact)
    }
    false
  }

  def isCast: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  def getInvokeExpr = new Nothing(delegate.getInvokeExpr, method)

  def isReturnStmt: Boolean = delegate.isInstanceOf[Nothing]

  def isThrowStmt: Boolean = delegate.isInstanceOf[Nothing]

  def isIfStmt: Boolean = delegate.isInstanceOf[Nothing]

  def getIfStmt = new Nothing(delegate.asInstanceOf[Nothing], method)

  // TODO Rename to getReturnOp();
  def getReturnOp: Nothing = {
    assert(isReturnStmt)
    val assignStmt = delegate.asInstanceOf[Nothing]
    new Nothing(assignStmt.getOp, method)
  }

  def isMultiArrayAllocation: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  def isStringAllocation: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  def isFieldStore: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getLeftOp.isInstanceOf[Nothing]

  def isArrayStore: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getLeftOp.isInstanceOf[Nothing]

  def isArrayLoad: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  def isFieldLoad: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  def isIdentityStmt: Boolean = delegate.isInstanceOf[Nothing]

  def getDelegate: Nothing = delegate

  def getShortLabel: Nothing = {
    if (delegate.isInstanceOf[Nothing]) {
      val assignStmt = delegate.asInstanceOf[Nothing]
      if (assignStmt.getRightOp.isInstanceOf[Nothing]) {
        val fr = assignStmt.getRightOp.asInstanceOf[Nothing]
        return assignStmt.getLeftOp + " = " + fr.getBase + "." + fr.getField.getName
      }
      if (assignStmt.getLeftOp.isInstanceOf[Nothing]) {
        val fr = assignStmt.getLeftOp.asInstanceOf[Nothing]
        return fr.getBase + "." + fr.getField.getName + " = " + assignStmt.getRightOp
      }
    }
    if (containsInvokeExpr) {
      val invokeExpr = getInvokeExpr
      if (invokeExpr.isStaticInvokeExpr) return (if (isAssign) getLeftOp + " = "
      else "") + invokeExpr.getMethod + "(" + invokeExpr.getArgs.toString.replace("[", "").replace("]", "") + ")"
      if (invokeExpr.isInstanceInvokeExpr) return (if (isAssign) getLeftOp + " = "
      else "") + invokeExpr.getBase + "." + invokeExpr.getMethod + "(" + invokeExpr.getArgs.toString.replace("[", "").replace("]", "") + ")"
    }
    delegate.toString
  }

  /**
   * This method kills a data-flow at an if-stmt, it is assumed that the propagated "allocation"
   * site is x = null and fact is the propagated aliased variable. (i.e., y after a statement y =
   * x). If the if-stmt checks for if y != null or if y == null, data-flow propagation can be killed
   * when along the true/false branch.
   *
   * @param fact      The data-flow value that bypasses the if-stmt
   * @param successor The successor statement of the if-stmt
   * @return true if the Val fact shall be killed
   */
  @deprecated def killAtIfStmt(fact: Nothing, successor: Nothing): Boolean = {
    //		IfStmt ifStmt = this.getIfStmt();
    //		if(successor instanceof CallSiteStatement) {
    //          successor = ((CallSiteStatement) successor).getDelegate();
    //		} else if(successor instanceof ReturnSiteStatement) {
    //          successor = ((ReturnSiteStatement) successor).getDelegate();
    //        }
    //		Stmt succ = ((JimpleStatement)successor).getDelegate();
    //		Stmt target = ifStmt.getTarget();
    //
    //		Value condition = ifStmt.getCondition();
    //		if (condition instanceof JEqExpr) {
    //			JEqExpr eqExpr = (JEqExpr) condition;
    //			Value op1 = eqExpr.getOp1();
    //			Value op2 = eqExpr.getOp2();
    //			Val jop1 = new JimpleVal(eqExpr.getOp1(), successor.getMethod());
    //			Val jop2 = new JimpleVal(eqExpr.getOp2(), successor.getMethod());
    //			if (fact instanceof JimpleDoubleVal) {
    //				JimpleDoubleVal valWithFalseVar = (JimpleDoubleVal) fact;
    //				if (jop1.equals(valWithFalseVar.getFalseVariable())) {
    //					if (op2.equals(IntConstant.v(0))) {
    //						if (!succ.equals(target)) {
    //							return true;
    //						}
    //					}
    //				}
    //				if (jop2.equals(valWithFalseVar.getFalseVariable())) {
    //					if (op1.equals(IntConstant.v(0))) {
    //						if (!succ.equals(target)) {
    //							return true;
    //						}
    //					}
    //				}
    //			}
    //			if (op1 instanceof NullConstant) {
    //				if (new JimpleVal(op2,successor.getMethod()).equals(fact)) {
    //					if (!succ.equals(target)) {
    //						return true;
    //					}
    //				}
    //			} else if (op2 instanceof NullConstant) {
    //				if (new JimpleVal(op1,successor.getMethod()).equals(fact)) {
    //					if (!succ.equals(target)) {
    //						return true;
    //					}
    //				}
    //			}
    //		}
    //		if (condition instanceof JNeExpr) {
    //			JNeExpr eqExpr = (JNeExpr) condition;
    //			Value op1 = eqExpr.getOp1();
    //			Value op2 = eqExpr.getOp2();
    //			if (op1 instanceof NullConstant) {
    //				if (new JimpleVal(op2,successor.getMethod()).equals(fact)) {
    //					if (succ.equals(target)) {
    //						return true;
    //					}
    //				}
    //			} else if (op2 instanceof NullConstant) {
    //				if (new JimpleVal(op1,successor.getMethod()).equals(fact)) {
    //					if (succ.equals(target)) {
    //						return true;
    //					}
    //				}
    //			}
    //		}
    false
  }

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
    val other = obj.asInstanceOf[JimpleStatement]
    if (delegate == null) other.delegate == null
    else delegate.equals(other.delegate)
  }

  @Override def getFieldStore: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    val `val` = ins.getLeftOp.asInstanceOf[Nothing]
    new Nothing(new Nothing(`val`.getBase, method), new Nothing(`val`.getField))
  }

  @Override def getFieldLoad: Nothing = {
    val ins = delegate.asInstanceOf[Nothing]
    val `val` = ins.getRightOp.asInstanceOf[Nothing]
    new Nothing(new Nothing(`val`.getBase, method), new Nothing(`val`.getField))
  }

  @Override def isStaticFieldLoad: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  @Override def isStaticFieldStore: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getLeftOp.isInstanceOf[Nothing]

  @Override def getStaticField: Nothing = {
    var v: Nothing = null
    if (isStaticFieldLoad) v = delegate.asInstanceOf[Nothing].getRightOp.asInstanceOf[Nothing]
    else if (isStaticFieldStore) v = delegate.asInstanceOf[Nothing].getLeftOp.asInstanceOf[Nothing]
    else throw new Nothing("Error")
    new Nothing(new Nothing(v.getField), method)
  }

  @Override def isPhiStatement = false

  @Override def getPhiVals = throw new Nothing("Not supported!")

  @Override def getArrayBase: Nothing = {
    if (isArrayLoad) {
      val rightOp = getRightOp
      return rightOp.getArrayBase
    }
    if (isArrayStore) {
      val rightOp = getLeftOp
      return rightOp.getArrayBase
    }
    throw new Nothing("Dead code")
  }

  @Override def getStartLineNumber: Int = delegate.getJavaSourceStartLineNumber

  @Override def getStartColumnNumber: Int = delegate.getJavaSourceStartColumnNumber

  @Override def getEndColumnNumber: Int = {
    // TODO move to Soot
    val tag = delegate.getTag("SourceLnPosTag").asInstanceOf[Nothing]
    if (tag != null) return tag.endPos
    -1
  }

  @Override def getEndLineNumber: Int = {
    // TODO move to Soot
    val tag = delegate.getTag("SourceLnPosTag").asInstanceOf[Nothing]
    if (tag != null) return tag.endLn
    -1
  }

  @Override def isCatchStmt: Boolean = delegate.isInstanceOf[Nothing] && delegate.asInstanceOf[Nothing].getRightOp.isInstanceOf[Nothing]

  def isUnitializedFieldStatement: Boolean = delegate.hasTag(BoomerangPretransformer.UNITIALIZED_FIELD_TAG_NAME)
}