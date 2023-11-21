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
package test

import boomerang.scene.Statement

object ExpectedResults {
  private[test] object InternalState extends Enumeration {
    type InternalState = Value
    val ERROR, ACCEPTING = Value
  }
}

abstract class ExpectedResults[State, Val] private[test](private[test] val unit: Nothing, private[test] val `val`: Val, private[test] val state: ExpectedResults.InternalState) extends Nothing with Nothing {
  protected var satisfied = false
  protected var imprecise = false

  def isSatisfied: Boolean = satisfied

  def isImprecise: Boolean = imprecise

  def getVal: Val = `val`

  def getStmt: Nothing = unit

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (`val` == null) 0
    else `val`.hashCode)
    result = prime * result + (if (state == null) 0
    else state.hashCode)
    result = prime * result + (if (unit == null) 0
    else unit.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ExpectedResults[_, _]]
    if (`val` == null) if (other.`val` != null) return false
    else if (!`val`.equals(other.`val`)) return false
    if (state ne other.state) return false
    if (unit == null) if (other.unit != null) return false
    else if (!unit.equals(other.unit)) return false
    true
  }

  @Override def toString: Nothing = "[" + `val` + " @ " + unit + " in state " + state + "]"
}