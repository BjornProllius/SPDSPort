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

object InputStreamStateMachine {
  private val CLOSE_METHODS = ".* close.*"
  private val READ_METHODS = ".* read.*"
  private val TYPE = "java.io.InputStream"

  final class States extends Nothing {}
}

class InputStreamStateMachine extends Nothing {
  addTransition(new Nothing(InputStreamStateMachine.States.CLOSED, InputStreamStateMachine.CLOSE_METHODS, Parameter.This, InputStreamStateMachine.States.CLOSED, Type.OnCall))
  addTransition(new Nothing(InputStreamStateMachine.States.CLOSED, InputStreamStateMachine.READ_METHODS, Parameter.This, InputStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(InputStreamStateMachine.States.ERROR, InputStreamStateMachine.READ_METHODS, Parameter.This, InputStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(InputStreamStateMachine.States.CLOSED, InputStreamStateMachine.READ_METHODS, Parameter.This, InputStreamStateMachine.States.ERROR, Type.OnCallToReturn))
  addTransition(new Nothing(InputStreamStateMachine.States.ERROR, InputStreamStateMachine.READ_METHODS, Parameter.This, InputStreamStateMachine.States.ERROR, Type.OnCallToReturn))

  @Override def generateSeed(edge: Nothing): Nothing = this.generateThisAtAnyCallSitesOf(edge, InputStreamStateMachine.TYPE, InputStreamStateMachine.CLOSE_METHODS)

  @Override def initialState: Nothing = InputStreamStateMachine.States.CLOSED
}