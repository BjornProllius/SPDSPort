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
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Type
import boomerang.scene.Val
import com.ibm.wala.analysis.typeInference.PointType
import com.ibm.wala.classLoader.IClass
import com.ibm.wala.classLoader.NewSiteReference
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.ssa.SSAArrayReferenceInstruction
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ssa.SSAInvokeInstruction
import com.ibm.wala.ssa.SymbolTable

object WALAVal {
  object OP extends Enumeration {
    type OP = Value
    val LEFT, RIGHT = Value
  }
}

class WALAVal extends Nothing {
  final private var programCounter = 0
  final protected var method: Nothing = null
  final private var symbolTable: Nothing = null
  final private var newSite: Nothing = null
  final private var ssaInstruction: Nothing = null
  final private var op: WALAVal.OP = null

  def this(programCounter: Int, method: Nothing, unbalanced: Nothing) {
    this()
    super (method, unbalanced)
    this.programCounter = programCounter
    this.method = method
    this.symbolTable = method.getIR.getSymbolTable
    this.newSite = null
    this.ssaInstruction = null
    this.op = null
  }

  def this(programCounter: Int, method: Nothing) {
    this(programCounter, method, null)
  }

  /**
   * WALAVal representing a new allocation site statement
   *
   * @param newSite
   * @param method
   */
  def this(newSite: Nothing, method: Nothing, unbalanced: Nothing) {
    this()
    super (method, unbalanced)
    this.newSite = newSite
    this.method = method
    this.symbolTable = null
    this.programCounter = -1
    this.ssaInstruction = null
    this.op = null
  }

  def this(newSite: Nothing, method: Nothing) {
    this(newSite, method, null)
  }

  /**
   * Dummy WALAVal to represent right hand side of any instruction for instance of a phi val or of
   * an assining call statement.
   *
   * @param delegate
   * @param method
   */
  def this(delegate: Nothing, op: WALAVal.OP, method: Nothing, unbalanced: Nothing) {
    this()
    super (method, unbalanced)
    this.ssaInstruction = delegate
    this.op = op
    this.method = method
    this.newSite = null
    this.symbolTable = null
    this.programCounter = -1
  }

  def this(delegate: Nothing, op: WALAVal.OP, method: Nothing) {
    this(delegate, op, method, null)
  }

  @Override def getType: Nothing = {
    if (newSite != null) return getNewExprType
    if (ssaInstruction.isInstanceOf[Nothing]) {
      val ssaInvokeInstruction = ssaInstruction.asInstanceOf[Nothing]
      return new Nothing(method.getTypeInference.getType(ssaInvokeInstruction.getDef))
    }
    new Nothing(method.getTypeInference.getType(programCounter))
  }

  @Override def isStatic: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def isNewExpr: Boolean = newSite != null

  @Override def getNewExprType: Nothing = {
    val cha = method.getIR.getMethod.getDeclaringClass.getClassHierarchy
    val klass = cha.lookupClass(newSite.getDeclaredType)
    new Nothing(new Nothing(klass))
  }

  @Override def asUnbalanced(stmt: Nothing): Nothing = {
    if (newSite != null) return new WALAVal(newSite, method, stmt)
    if (ssaInstruction != null) return new WALAVal(ssaInstruction, op, method, stmt)
    new WALAVal(programCounter, method, stmt)
  }

  @Override def isLocal = true

  @Override def isArrayAllocationVal: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def isNull: Boolean = symbolTable != null && symbolTable.getValue(programCounter) != null && symbolTable.getValue(programCounter).isNullConstant

  @Override def isStringConstant: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getStringValue: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def isStringBufferOrBuilder: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def isThrowableAllocationType: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def isCast: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getCastOp: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def isArrayRef: Boolean = ssaInstruction.isInstanceOf[Nothing]

  @Override def isInstanceOfExpr: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getInstanceOfOp: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def isLengthExpr: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getLengthOp: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def isIntConstant: Boolean = {
    if (symbolTable == null) return false
    if (programCounter == -1) return false
    symbolTable.isIntegerConstant(programCounter)
  }

  @Override def isClassConstant = false

  @Override def getClassConstantType: Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def withNewMethod(callee: Nothing): Nothing = {
    // TODO Auto-generated method stub
    null
  }

  @Override def toString: Nothing = Integer.valueOf(programCounter).toString + " in " + method.toString

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (method == null) 0
    else method.hashCode)
    result = prime * result + (if (newSite == null) 0
    else newSite.hashCode)
    result = prime * result + (if (op == null) 0
    else op.hashCode)
    result = prime * result + (if (ssaInstruction == null) 0
    else ssaInstruction.hashCode)
    result = prime * result + programCounter
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[WALAVal]
    if (method == null) if (other.method != null) return false
    else if (!method.equals(other.method)) return false
    if (newSite == null) if (other.newSite != null) return false
    else if (!newSite.equals(other.newSite)) return false
    if (op == null) if (other.op != null) return false
    else if (!op.equals(other.op)) return false
    if (ssaInstruction == null) if (other.ssaInstruction != null) return false
    else if (!ssaInstruction.equals(other.ssaInstruction)) return false
    if (programCounter != other.programCounter) return false
    true
  }

  @Override def isLongConstant: Boolean = {
    // TODO Auto-generated method stub
    false
  }

  @Override def getIntValue: Int = symbolTable.getIntValue(programCounter)

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