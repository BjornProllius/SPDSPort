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

class ForwardQueryMultiDimensionalArray(edge: ControlFlowGraph.Edge, variable: Val, index1: Integer, index2: Integer) 
  extends ForwardQueryArray(edge, variable, index1) {

  override def toString: String = "ArrayForwardQuery: " + super.toString + " Index1: " + getIndex1 + " Index2: " + index2

  override def equals(o: Any): Boolean = {
    o match {
      case that: ForwardQueryMultiDimensionalArray =>
        super.equals(that) && Objects.equal(index2, that.index2)
      case _ => false
    }
  }

  override def hashCode: Int = Objects.hashCode(super.hashCode, index2)

  def getIndex1: Integer = getIndex

  def getIndex2: Integer = index2
}