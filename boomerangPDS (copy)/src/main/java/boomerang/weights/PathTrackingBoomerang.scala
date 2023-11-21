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

import boomerang.BoomerangOptions
import boomerang.ForwardQuery
import boomerang.WeightedBoomerang
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Field
import boomerang.scene.Val
import sync.pds.solver.OneWeightFunctions
import sync.pds.solver.WeightFunctions

abstract class PathTrackingBoomerang extends Nothing {
  private var fieldWeights: Nothing = null
  private var callWeights: Nothing = null

  def this(cg: Nothing, scope: Nothing) {
    this()
    super (cg, scope)
  }

  def this(cg: Nothing, scope: Nothing, opt: Nothing) {
    this()
    super (cg, scope, opt)
  }

  @Override protected def getForwardFieldWeights: Nothing = getOrCreateFieldWeights

  @Override protected def getBackwardFieldWeights: Nothing = getOrCreateFieldWeights

  @Override protected def getBackwardCallWeights: Nothing = getOrCreateCallWeights

  @Override protected def getForwardCallWeights(sourceQuery: Nothing): Nothing = getOrCreateCallWeights

  private def getOrCreateFieldWeights = {
    if (fieldWeights == null) fieldWeights = new Nothing(DataFlowPathWeight.one)
    fieldWeights
  }

  private def getOrCreateCallWeights = {
    if (callWeights == null) callWeights = new Nothing(options.trackDataFlowPath, options.trackPathConditions, options.trackImplicitFlows)
    callWeights
  }
}