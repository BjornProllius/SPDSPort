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
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import java.util.Collections
import java.util
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type
import typestate.finiteautomata.State
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object HasNextStateMachine {
  private val NEXT_METHOD = ".* next\\(\\)"
  private val HAS_NEXT_METHOD = ".* hasNext\\(\\)"

  final class States extends Nothing {}
}

class HasNextStateMachine extends Nothing {
  addTransition(new Nothing(HasNextStateMachine.States.INIT, HasNextStateMachine.NEXT_METHOD, Parameter.This, HasNextStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(HasNextStateMachine.States.ERROR, HasNextStateMachine.NEXT_METHOD, Parameter.This, HasNextStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(HasNextStateMachine.States.HASNEXT, HasNextStateMachine.NEXT_METHOD, Parameter.This, HasNextStateMachine.States.INIT, Type.OnCall))
  addTransition(new Nothing(HasNextStateMachine.States.INIT, HasNextStateMachine.HAS_NEXT_METHOD, Parameter.This, HasNextStateMachine.States.HASNEXT, Type.OnCall))
  addTransition(new Nothing(HasNextStateMachine.States.HASNEXT, HasNextStateMachine.HAS_NEXT_METHOD, Parameter.This, HasNextStateMachine.States.HASNEXT, Type.OnCall))
  addTransition(new Nothing(HasNextStateMachine.States.ERROR, HasNextStateMachine.HAS_NEXT_METHOD, Parameter.This, HasNextStateMachine.States.ERROR, Type.OnCall))

  def generateSeed(edge: Nothing): Nothing = {
    val unit = edge.getStart
    if (unit.containsInvokeExpr && unit.isAssign) {
      val invokeExpr = unit.getInvokeExpr
      if (invokeExpr.isInstanceInvokeExpr) if (invokeExpr.getMethod.getName.contains("iterator")) return Collections.singleton(new Nothing(edge, new Nothing(unit.getLeftOp, unit, unit.getLeftOp), initialTransition))
    }
    Collections.emptySet
  }

  @Override def initialState: Nothing = HasNextStateMachine.States.INIT
}