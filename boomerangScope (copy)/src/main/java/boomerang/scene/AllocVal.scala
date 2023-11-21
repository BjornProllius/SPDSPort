package boomerang.scene

import boomerang.scene.ControlFlowGraph.Edge

class AllocVal(private var delegate: Nothing, private var allocStatement: Nothing, private var allocationVal: Nothing) extends Nothing {
  def getType: Nothing = delegate.getType

  def m: Nothing = delegate.m

  def toString: Nothing = {
    if (allocStatement.isAssign) {
      if (allocStatement.getRightOp.isIntConstant) return delegate + " Value (int): " + allocStatement.getRightOp.getIntValue
      if (allocStatement.getRightOp.isStringConstant) return delegate + " Value (String): " + allocStatement.getRightOp.getStringValue
    }
    if (delegate == null) return ""
    delegate.toString
  }

  def isStatic: Boolean = delegate.isStatic

  def isNewExpr: Boolean = delegate.isNewExpr

  def getNewExprType: Nothing = delegate.getNewExprType

  def isUnbalanced: Boolean = delegate.isUnbalanced

  def asUnbalanced(stmt: Nothing): Nothing = delegate.asUnbalanced(stmt)

  def isLocal: Boolean = delegate.isLocal

  def isArrayAllocationVal: Boolean = delegate.isArrayAllocationVal

  def isNull: Boolean = allocationVal.isNull

  def isStringConstant: Boolean = delegate.isStringConstant

  def getStringValue: Nothing = delegate.getStringValue

  def isStringBufferOrBuilder: Boolean = delegate.isStringBufferOrBuilder

  def isThrowableAllocationType: Boolean = delegate.isThrowableAllocationType

  def isCast: Boolean = delegate.isCast

  def getCastOp: Nothing = delegate.getCastOp

  def isArrayRef: Boolean = delegate.isArrayRef

  def isInstanceOfExpr: Boolean = delegate.isInstanceOfExpr

  def getInstanceOfOp: Nothing = delegate.getInstanceOfOp

  def isLengthExpr: Boolean = delegate.isLengthExpr

  def getLengthOp: Nothing = delegate.getLengthOp

  def isIntConstant: Boolean = delegate.isIntConstant

  def isClassConstant: Boolean = delegate.isClassConstant

  def getClassConstantType: Nothing = delegate.getClassConstantType

  def withNewMethod(callee: Nothing): Nothing = delegate.withNewMethod(callee)

  def withSecondVal(leftOp: Nothing): Nothing = delegate.withSecondVal(leftOp)

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (allocStatement == null) 0
    else allocStatement.hashCode)
    result = prime * result + (if (allocationVal == null) 0
    else allocationVal.hashCode)
    result = prime * result + (if (delegate == null) 0
    else delegate.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[AllocVal]
    if (allocStatement == null) if (other.allocStatement != null) return false
    else if (!allocStatement.equals(other.allocStatement)) return false
    if (allocationVal == null) if (other.allocationVal != null) return false
    else if (!allocationVal.equals(other.allocationVal)) return false
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  def getAllocVal: Nothing = allocationVal

  def getDelegate: Nothing = delegate

  @Override def isLongConstant = false

  @Override def getIntValue: Int = delegate.getIntValue

  @Override def getLongValue: Long = delegate.getLongValue

  @Override def getArrayBase: Nothing = delegate.getArrayBase

  @Override def getVariableName: Nothing = delegate.getVariableName
}