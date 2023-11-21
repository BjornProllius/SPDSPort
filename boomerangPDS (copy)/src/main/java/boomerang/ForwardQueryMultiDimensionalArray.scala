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
package boomerang

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import com.google.common.base.Objects

class ForwardQueryMultiDimensionalArray(stmt: Nothing, variable: Nothing, index1: Nothing, private val index2: Nothing) extends Nothing(stmt, variable, index1) {
  @Override def toString: Nothing = "ArrayForwardQuery: " + super.toString + " Index1: " + getIndex1 + " Index2: " + index2

  @Override def equals(o: Nothing): Boolean = {
    if (this eq o) return true
    if (!super.equals(o)) return false
    val that = o.asInstanceOf[ForwardQueryMultiDimensionalArray]
    Objects.equal(index2, that.index2)
  }

  @Override def hashCode: Int = Objects.hashCode(super.hashCode, index2)

  def getIndex1: Nothing = getIndex

  def getIndex2: Nothing = index2
}