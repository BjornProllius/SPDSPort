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

object PipedInputStreamStateMachine {
  private val CONNECT_METHODS = "connect"
  private val READ_METHODS = "read"

  final class States extends Nothing {}
}

class PipedInputStreamStateMachine extends Nothing {
  addTransition(new Nothing(PipedInputStreamStateMachine.States.INIT, PipedInputStreamStateMachine.CONNECT_METHODS, Parameter.This, PipedInputStreamStateMachine.States.CONNECTED, Type.OnCall))
  addTransition(new Nothing(PipedInputStreamStateMachine.States.INIT, PipedInputStreamStateMachine.READ_METHODS, Parameter.This, PipedInputStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(PipedInputStreamStateMachine.States.CONNECTED, PipedInputStreamStateMachine.READ_METHODS, Parameter.This, PipedInputStreamStateMachine.States.CONNECTED, Type.OnCall))
  addTransition(new Nothing(PipedInputStreamStateMachine.States.ERROR, PipedInputStreamStateMachine.READ_METHODS, Parameter.This, PipedInputStreamStateMachine.States.ERROR, Type.OnCall))

  //    private Set<SootMethod> connect() {
  //        return selectMethodByName(getSubclassesOf("java.io.PipedInputStream"), "connect");
  //    }
  //
  //    private Set<SootMethod> readMethods() {
  //        return selectMethodByName(getSubclassesOf("java.io.PipedInputStream"), "read");
  //    }
  @Override def generateSeed(unit: Nothing): Nothing = generateAtAllocationSiteOf(unit, classOf[Nothing])

  @Override protected def initialState: Nothing = PipedInputStreamStateMachine.States.INIT
}