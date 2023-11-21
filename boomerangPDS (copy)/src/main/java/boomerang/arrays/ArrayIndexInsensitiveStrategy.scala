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
import boomerang.scene.Field
import boomerang.scene.Pair
import boomerang.scene.Val
import java.util
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.nodes.PushNode
import wpds.impl.Weight
import wpds.interfaces.State

class ArrayIndexInsensitiveStrategy[W <: Weight] extends Nothing {
  @Override def handleForward(curr: Nothing, arrayBase: Nothing, out: Nothing): Unit = {
    out.add(new Nothing(curr, arrayBase.getX, Field.array(-1), PDSSystem.FIELDS))
  }

  @Override def handleBackward(curr: Nothing, arrayBase: Nothing, out: Nothing): Unit = {
    out.add(new Nothing(curr, arrayBase.getX, Field.array(-1), PDSSystem.FIELDS))
  }
}