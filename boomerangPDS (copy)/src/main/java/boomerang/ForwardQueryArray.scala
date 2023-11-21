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

class ForwardQueryArray(stmt: Nothing, variable: Nothing, private val index: Nothing) extends Nothing(stmt, variable) {
  @Override def toString: Nothing = "ArrayForwardQuery: " + super.toString + " Index: " + index

  @Override def equals(o: Nothing): Boolean = {
    if (this eq o) return true
    if (!super.equals(o)) return false
    val that = o.asInstanceOf[ForwardQueryArray]
    Objects.equal(index, that.index)
  }

  @Override def hashCode: Int = Objects.hashCode(super.hashCode, index)

  def getIndex: Nothing = index
}