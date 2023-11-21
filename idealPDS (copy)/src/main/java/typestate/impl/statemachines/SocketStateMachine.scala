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
import boomerang.scene.DeclaredMethod
import java.net.Socket
import java.util
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type
import typestate.finiteautomata.State
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object SocketStateMachine {
  private val CONNECT_METHOD = ".* connect.*"

  final class States extends Nothing {}

  private class UseMethodMatcher(from: Nothing, param: Nothing, to: Nothing, `type`: Nothing) extends Nothing(from, CONNECT_METHOD, param, to, `type`) {
    @Override def matches(declaredMethod: Nothing): Boolean = {
      if (super.matches(declaredMethod)) return false
      val isSocketMethod = declaredMethod.getDeclaringClass.getFullyQualifiedName.equals("java.net.Socket")
      if (!isSocketMethod) return false
      val methodName = declaredMethod.getName
      val isConstructor = methodName.contains("<init>")
      val isSetImpl = methodName.startsWith("setImpl")
      !(isConstructor || isSetImpl)
    }
  }
}

class SocketStateMachine extends Nothing {
  addTransition(new Nothing(SocketStateMachine.States.INIT, SocketStateMachine.CONNECT_METHOD, Parameter.This, SocketStateMachine.States.CONNECTED, Type.OnCallOrOnCallToReturn))
  addTransition(new Nothing(SocketStateMachine.States.ERROR, SocketStateMachine.CONNECT_METHOD, Parameter.This, SocketStateMachine.States.ERROR, Type.OnCallOrOnCallToReturn))
  addTransition(new SocketStateMachine.UseMethodMatcher(SocketStateMachine.States.CONNECTED, Parameter.This, SocketStateMachine.States.CONNECTED, Type.OnCallOrOnCallToReturn))
  addTransition(new SocketStateMachine.UseMethodMatcher(SocketStateMachine.States.INIT, Parameter.This, SocketStateMachine.States.ERROR, Type.OnCallOrOnCallToReturn))
  addTransition(new Nothing(SocketStateMachine.States.CONNECTED, SocketStateMachine.CONNECT_METHOD, Parameter.This, SocketStateMachine.States.CONNECTED, Type.OnCallOrOnCallToReturn))
  addTransition(new SocketStateMachine.UseMethodMatcher(SocketStateMachine.States.ERROR, Parameter.This, SocketStateMachine.States.ERROR, Type.OnCallOrOnCallToReturn))

  @Override def generateSeed(unit: Nothing): Nothing = generateAtAllocationSiteOf(unit, classOf[Nothing])

  @Override protected def initialState: Nothing = SocketStateMachine.States.INIT
}