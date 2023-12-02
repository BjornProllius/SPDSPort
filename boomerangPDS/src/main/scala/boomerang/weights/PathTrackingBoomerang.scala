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
package boomerang.weights

import boomerang.scene.{CallGraph, ControlFlowGraph, DataFlowScope, Field, Val}
import boomerang.{BoomerangOptions, ForwardQuery, WeightedBoomerang}
import sync.pds.solver.{OneWeightFunctions, WeightFunctions}

abstract class PathTrackingBoomerang(cg: CallGraph, scope: DataFlowScope, opt: BoomerangOptions)
  extends WeightedBoomerang[DataFlowPathWeight](cg, scope, opt) {

  private var fieldWeights: OneWeightFunctions[ControlFlowGraph.Edge, Val, Field, DataFlowPathWeight] = _
  private var callWeights: PathTrackingWeightFunctions = _

  override protected def getForwardFieldWeights: WeightFunctions[ControlFlowGraph.Edge, Val, Field, DataFlowPathWeight] = {
    getOrCreateFieldWeights
  }

  override protected def getBackwardFieldWeights: WeightFunctions[ControlFlowGraph.Edge, Val, Field, DataFlowPathWeight] = {
    getOrCreateFieldWeights
  }

  override protected def getBackwardCallWeights: WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, DataFlowPathWeight] = {
    getOrCreateCallWeights
  }

  override protected def getForwardCallWeights(sourceQuery: ForwardQuery): WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, DataFlowPathWeight] = {
    getOrCreateCallWeights
  }

  private def getOrCreateFieldWeights: WeightFunctions[ControlFlowGraph.Edge, Val, Field, DataFlowPathWeight] = {
    if (fieldWeights == null) {
      fieldWeights = new OneWeightFunctions[ControlFlowGraph.Edge, Val, Field, DataFlowPathWeight](DataFlowPathWeight.one)
    }
    fieldWeights
  }

  private def getOrCreateCallWeights: WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, DataFlowPathWeight] = {
    if (callWeights == null) {
      callWeights = new PathTrackingWeightFunctions(
        options.trackDataFlowPath,
        options.trackPathConditions,
        options.trackImplicitFlows
      )
    }
    callWeights
  }
}