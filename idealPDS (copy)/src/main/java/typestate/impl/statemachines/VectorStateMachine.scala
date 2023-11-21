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
import java.util.Collections
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type
import typestate.finiteautomata.State
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object VectorStateMachine {
  private val ADD_ELEMENT_METHODS = "(.* (add|addAll|addElement|insertElementAt|set|setElementAt).*)|<java.util.Vector: void <init>(java.util.Collection)>"
  private val ACCESS_ELEMENT_METHODS = ".* (elementAt|firstElement|lastElement|get).*"
  private val REMOVE_ALL_METHODS = ".* removeAllElements.*"

  final class States extends Nothing {}
}

class VectorStateMachine extends Nothing {
  addTransition(new Nothing(VectorStateMachine.States.INIT, VectorStateMachine.ADD_ELEMENT_METHODS, Parameter.This, VectorStateMachine.States.NOT_EMPTY, Type.OnCall))
  addTransition(new Nothing(VectorStateMachine.States.INIT, VectorStateMachine.ACCESS_ELEMENT_METHODS, Parameter.This, VectorStateMachine.States.ACCESSED_EMPTY, Type.OnCall))
  addTransition(new Nothing(VectorStateMachine.States.NOT_EMPTY, VectorStateMachine.ACCESS_ELEMENT_METHODS, Parameter.This, VectorStateMachine.States.NOT_EMPTY, Type.OnCall))
  addTransition(new Nothing(VectorStateMachine.States.NOT_EMPTY, VectorStateMachine.REMOVE_ALL_METHODS, Parameter.This, VectorStateMachine.States.INIT, Type.OnCall))
  addTransition(new Nothing(VectorStateMachine.States.INIT, VectorStateMachine.REMOVE_ALL_METHODS, Parameter.This, VectorStateMachine.States.INIT, Type.OnCall))
  addTransition(new Nothing(VectorStateMachine.States.ACCESSED_EMPTY, VectorStateMachine.ACCESS_ELEMENT_METHODS, Parameter.This, VectorStateMachine.States.ACCESSED_EMPTY, Type.OnCall))

  @Override def generateSeed(unit: Nothing): Nothing = {
    if (unit.getMethod.toString.contains("<clinit>")) return Collections.emptySet
    generateAtAllocationSiteOf(unit, classOf[Nothing])
  }

  @Override protected def initialState: Nothing = VectorStateMachine.States.INIT
}