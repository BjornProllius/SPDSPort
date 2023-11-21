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

import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Field
import boomerang.scene.Val
import sync.pds.solver.OneWeightFunctions
import sync.pds.solver.WeightFunctions
import wpds.impl.Weight

class Boomerang extends Nothing {
  private var fieldWeights: Nothing = null
  private var callWeights: Nothing = null

  def this(callGraph: Nothing, scope: Nothing) {
    this()
    super (callGraph, scope)
  }

  def this(callGraph: Nothing, scope: Nothing, opt: Nothing) {
    this()
    super (callGraph, scope, opt)
  }

  @Override protected def getForwardFieldWeights: Nothing = getOrCreateFieldWeights

  @Override protected def getBackwardFieldWeights: Nothing = getOrCreateFieldWeights

  @Override protected def getBackwardCallWeights: Nothing = getOrCreateCallWeights

  @Override protected def getForwardCallWeights(sourceQuery: Nothing): Nothing = getOrCreateCallWeights

  private def getOrCreateFieldWeights = {
    if (fieldWeights == null) fieldWeights = new Nothing(Weight.NO_WEIGHT_ONE)
    fieldWeights
  }

  private def getOrCreateCallWeights = {
    if (callWeights == null) callWeights = new Nothing(Weight.NO_WEIGHT_ONE)
    callWeights
  }
}