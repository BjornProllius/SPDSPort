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

import boomerang.scene.{ControlFlowGraph, Val}
import com.google.common.base.Objects

class ForwardQueryArray(edge: ControlFlowGraph.Edge, variable: Val, index: Integer) extends ForwardQuery(edge, variable) {

  override def toString: String = "ArrayForwardQuery: " + super.toString + " Index: " + index

  override def equals(o: Any): Boolean = {
    o match {
      case that: ForwardQueryArray =>
        super.equals(that) && Objects.equal(index, that.index)
      case _ => false
    }
  }

  override def hashCode: Int = Objects.hashCode(super.hashCode, index)

  def getIndex: Integer = index
}