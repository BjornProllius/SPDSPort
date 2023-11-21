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
package inference

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Val
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node

class InferenceWeightFunctions extends Nothing {
  @Override def push(curr: Nothing, succ: Nothing, field: Nothing): Nothing = {
    val callee = succ.stmt.getMethod
    if (!callee.isStatic) {
      val thisLocal = callee.getThisLocal
      if (succ.fact.equals(thisLocal)) return new Nothing(callee)
    }
    getOne
  }

  @Override def normal(curr: Nothing, succ: Nothing): Nothing = getOne

  @Override def pop(curr: Nothing): Nothing = getOne

  @Override def getOne: Nothing = InferenceWeight.one
}