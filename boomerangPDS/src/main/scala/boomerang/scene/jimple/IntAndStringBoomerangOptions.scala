package boomerang.scene.jimple

import boomerang.scene.{AllocVal, DeclaredMethod, Method, Statement, Val}
import boomerang.DefaultBoomerangOptions

import java.util.Optional

class IntAndStringBoomerangOptions extends DefaultBoomerangOptions {

  override def isAllocationVal(`val`: Val): Boolean = {
    if (`val`.isIntConstant()) {
      return true
    }
    super.isAllocationVal(`val`)
  }

  protected def isArrayAllocationVal(`val`: Val): Boolean = {
    `val`.isArrayAllocationVal()
  }

  override def getAllocationVal(m: Method, stmt: Statement, fact: Val): Optional[AllocVal] = {
    if (!stmt.isAssign()) {
      return Optional.empty()
    }
    if (!stmt.getLeftOp().equals(fact)) {
      return Optional.empty()
    }
    if (stmt.getRightOp().isLengthExpr()) {
      return Optional.of(new AllocVal(stmt.getLeftOp(), stmt, stmt.getRightOp()))
    }

    if (stmt.getRightOp().isIntConstant()) {
      return Optional.of(new AllocVal(stmt.getLeftOp(), stmt, stmt.getRightOp()))
    }
    if (stmt.containsInvokeExpr()) {
      val method = stmt.getInvokeExpr().getMethod()
      if (method.toString().equals("<java.math.BigInteger: java.math.BigInteger valueOf(long)>")) {
        val arg = stmt.getInvokeExpr().getArg(0)
        return Optional.of(new AllocVal(stmt.getLeftOp(), stmt, arg))
      }
    }
    super.getAllocationVal(m, stmt, fact)
  }

  override def trackStrings(): Boolean = {
    true
  }
}