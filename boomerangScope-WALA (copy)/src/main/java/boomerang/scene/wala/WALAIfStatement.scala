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

import boomerang.scene.IfStatement
import boomerang.scene.Statement
import boomerang.scene.Val
import com.ibm.wala.shrikeBT.IConditionalBranchInstruction.Operator
import com.ibm.wala.ssa.SSAConditionalBranchInstruction

class WALAIfStatement(private var delegate: Nothing, private var method: Nothing) extends Nothing {
  this.target = method.getBranchTarget(delegate.getTarget)
  private var target: Nothing = null

  @Override def getTarget: Nothing = target

  @Override def evaluate(`val`: Nothing): Nothing = {
    val op1 = new Nothing(delegate.getUse(0), method)
    val op2 = new Nothing(delegate.getUse(1), method)
    if (delegate.getOperator.equals(Operator.NE)) if ((`val`.equals(op1) && op2.isNull) || (`val`.equals(op2) && op1.isNull)) return Evaluation.FALSE
    if (delegate.getOperator.equals(Operator.EQ)) if ((`val`.equals(op1) && op2.isNull) || (`val`.equals(op2) && op1.isNull)) return Evaluation.TRUE
    Evaluation.UNKOWN
  }

  @Override def uses(`val`: Nothing): Boolean = {
    val op1 = new Nothing(delegate.getUse(0), method)
    val op2 = new Nothing(delegate.getUse(1), method)
    `val`.equals(op1) || `val`.equals(op2)
  }
}