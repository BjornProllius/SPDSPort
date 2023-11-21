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

object OutputStreamStateMachine {
  private val CLOSE_METHODS = ".* close.*"
  private val WRITE_METHODS = ".* write.*"
  private val TYPE = "java.io.OutputStream"

  final class States extends Nothing {}
}

class OutputStreamStateMachine extends Nothing {
  addTransition(new Nothing(OutputStreamStateMachine.States.NONE, OutputStreamStateMachine.CLOSE_METHODS, Parameter.This, OutputStreamStateMachine.States.CLOSED, Type.OnCall))
  addTransition(new Nothing(OutputStreamStateMachine.States.CLOSED, OutputStreamStateMachine.CLOSE_METHODS, Parameter.This, OutputStreamStateMachine.States.CLOSED, Type.OnCall))
  addTransition(new Nothing(OutputStreamStateMachine.States.CLOSED, OutputStreamStateMachine.WRITE_METHODS, Parameter.This, OutputStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(OutputStreamStateMachine.States.ERROR, OutputStreamStateMachine.WRITE_METHODS, Parameter.This, OutputStreamStateMachine.States.ERROR, Type.OnCall))

  @Override def generateSeed(edge: Nothing): Nothing = generateThisAtAnyCallSitesOf(edge, OutputStreamStateMachine.TYPE, OutputStreamStateMachine.CLOSE_METHODS)

  @Override protected def initialState: Nothing = OutputStreamStateMachine.States.NONE
}