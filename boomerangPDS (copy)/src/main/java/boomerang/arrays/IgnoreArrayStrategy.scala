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

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Pair
import boomerang.scene.Val
import java.util
import wpds.impl.Weight
import wpds.interfaces.State

class IgnoreArrayStrategy[W <: Weight] extends Nothing {
  @Override def handleForward(storeStmt: Nothing, storedVal: Nothing, out: Nothing): Unit = {
  }

  @Override def handleBackward(curr: Nothing, arrayBase: Nothing, out: Nothing): Unit = {
  }
}