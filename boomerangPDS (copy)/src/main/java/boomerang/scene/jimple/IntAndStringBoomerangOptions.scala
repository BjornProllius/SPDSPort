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

import boomerang.DefaultBoomerangOptions
import boomerang.scene.AllocVal
import boomerang.scene.DeclaredMethod
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import java.util.Optional

class IntAndStringBoomerangOptions extends Nothing {
  def isAllocationVal(`val`: Nothing): Boolean = {
    if (`val`.isIntConstant) return true
    super.isAllocationVal(`val`)
  }

  protected def isArrayAllocationVal(`val`: Nothing): Boolean = `val`.isArrayAllocationVal

  @Override def getAllocationVal(m: Nothing, stmt: Nothing, fact: Nothing): Nothing = {
    if (!stmt.isAssign) return Optional.empty
    if (!stmt.getLeftOp.equals(fact)) return Optional.empty
    if (stmt.getRightOp.isLengthExpr) return Optional.of(new Nothing(stmt.getLeftOp, stmt, stmt.getRightOp))
    if (stmt.getRightOp.isIntConstant) return Optional.of(new Nothing(stmt.getLeftOp, stmt, stmt.getRightOp))
    if (stmt.containsInvokeExpr) {
      // AtomicReference<AllocVal> returnValue = new AtomicReference<>();
      // icfg.addCalleeListener(new AllocationValCalleeListener(returnValue, as, icfg, m));
      // if (returnValue.get() != null){
      // return Optional.of(returnValue.get());
      // }
      val method = stmt.getInvokeExpr.getMethod
      if (method.toString.equals("<java.math.BigInteger: java.math.BigInteger valueOf(long)>")) {
        val arg = stmt.getInvokeExpr.getArg(0)
        return Optional.of(new Nothing(stmt.getLeftOp, stmt, arg))
      }
    }
    super.getAllocationVal(m, stmt, fact)
  }

  @Override def trackStrings = true
}