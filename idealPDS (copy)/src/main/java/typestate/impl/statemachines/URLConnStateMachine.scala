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
package typestate.impl.statemachines

import boomerang.WeightedForwardQuery
import boomerang.scene.ControlFlowGraph.Edge
import java.util
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type
import typestate.finiteautomata.State
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object URLConnStateMachine {
  private val CONNECT_METHOD = ".* connect.*"
  private val TYPE = "java.net.URLConnection"
  private val ILLEGAL_OPERTIONS = ".* (setDoInput|setDoOutput|setAllowUserInteraction|setUseCaches|setIfModifiedSince|setRequestProperty|addRequestProperty|getRequestProperty|getRequestProperties).*"

  final class States extends Nothing {}
}

class URLConnStateMachine extends Nothing {
  addTransition(new Nothing(URLConnStateMachine.States.CONNECTED, URLConnStateMachine.ILLEGAL_OPERTIONS, Parameter.This, URLConnStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(URLConnStateMachine.States.ERROR, URLConnStateMachine.ILLEGAL_OPERTIONS, Parameter.This, URLConnStateMachine.States.ERROR, Type.OnCall))

  @Override def generateSeed(unit: Nothing): Nothing = this.generateThisAtAnyCallSitesOf(unit, URLConnStateMachine.TYPE, URLConnStateMachine.CONNECT_METHOD)

  @Override protected def initialState: Nothing = URLConnStateMachine.States.CONNECTED
}