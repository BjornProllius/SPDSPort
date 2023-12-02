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
package boomerang

import boomerang.scene.{CallGraph, ControlFlowGraph, DataFlowScope, Field, Val}
import sync.pds.solver.{OneWeightFunctions, WeightFunctions}
import wpds.impl.Weight

class Boomerang(callGraph: CallGraph, scope: DataFlowScope, opt: BoomerangOptions)
  extends WeightedBoomerang[Weight.NoWeight](callGraph, scope, opt) {

  private var fieldWeights: OneWeightFunctions[ControlFlowGraph.Edge, Val, Field, Weight.NoWeight] = _
  private var callWeights: OneWeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, Weight.NoWeight] = _

  override protected def getForwardFieldWeights: WeightFunctions[ControlFlowGraph.Edge, Val, Field, Weight.NoWeight] = {
    getOrCreateFieldWeights
  }

  override protected def getBackwardFieldWeights: WeightFunctions[ControlFlowGraph.Edge, Val, Field, Weight.NoWeight] = {
    getOrCreateFieldWeights
  }

  override protected def getBackwardCallWeights: WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, Weight.NoWeight] = {
    getOrCreateCallWeights
  }

  override protected def getForwardCallWeights(sourceQuery: ForwardQuery): WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, Weight.NoWeight] = {
    getOrCreateCallWeights
  }

  private def getOrCreateFieldWeights: WeightFunctions[ControlFlowGraph.Edge, Val, Field, Weight.NoWeight] = {
    if (fieldWeights == null) {
      fieldWeights = new OneWeightFunctions[ControlFlowGraph.Edge, Val, Field, Weight.NoWeight](Weight.NO_WEIGHT_ONE)
    }
    fieldWeights
  }

  private def getOrCreateCallWeights: WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, Weight.NoWeight] = {
    if (callWeights == null) {
      callWeights = new OneWeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, Weight.NoWeight](Weight.NO_WEIGHT_ONE)
    }
    callWeights
  }
}