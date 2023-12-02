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
package boomerang.arrays

import boomerang.scene.{ControlFlowGraph, Pair, Val}
import wpds.impl.Weight
import wpds.interfaces.State
import scala.collection.Set

class IgnoreArrayStrategy[W <: Weight] extends ArrayHandlingStrategy[W] {

  override def handleForward(storeStmt: ControlFlowGraph.Edge, storedVal: Pair[Val, Int], out: Set[State]): Unit = {}

  override def handleBackward(curr: ControlFlowGraph.Edge, arrayBase: Pair[Val, Int], out: Set[State]): Unit = {}
}