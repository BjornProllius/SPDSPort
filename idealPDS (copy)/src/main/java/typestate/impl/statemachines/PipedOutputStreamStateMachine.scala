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

object PipedOutputStreamStateMachine {
  final class States extends Nothing {}

  private val CONNECT_METHODS = "connect"
  private val WRITE_METHODS = "write"
}

class PipedOutputStreamStateMachine extends Nothing {
  addTransition(new Nothing(PipedOutputStreamStateMachine.States.INIT, PipedOutputStreamStateMachine.CONNECT_METHODS, Parameter.This, PipedOutputStreamStateMachine.States.CONNECTED, Type.OnCall))
  addTransition(new Nothing(PipedOutputStreamStateMachine.States.INIT, PipedOutputStreamStateMachine.WRITE_METHODS, Parameter.This, PipedOutputStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(PipedOutputStreamStateMachine.States.CONNECTED, PipedOutputStreamStateMachine.WRITE_METHODS, Parameter.This, PipedOutputStreamStateMachine.States.CONNECTED, Type.OnCall))
  addTransition(new Nothing(PipedOutputStreamStateMachine.States.ERROR, PipedOutputStreamStateMachine.WRITE_METHODS, Parameter.This, PipedOutputStreamStateMachine.States.ERROR, Type.OnCall))

  @Override def generateSeed(stmt: Nothing): Nothing = generateAtAllocationSiteOf(stmt, classOf[Nothing])

  @Override protected def initialState: Nothing = PipedOutputStreamStateMachine.States.INIT
}