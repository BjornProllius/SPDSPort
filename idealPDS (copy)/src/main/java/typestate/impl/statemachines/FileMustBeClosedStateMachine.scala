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

object FileMustBeClosedStateMachine {
  final class States extends Nothing {}
}

class FileMustBeClosedStateMachine extends Nothing {
  addTransition(new Nothing(FileMustBeClosedStateMachine.States.INIT, ".*open.*", Parameter.This, FileMustBeClosedStateMachine.States.OPENED, Type.OnCall))
  addTransition(new Nothing(FileMustBeClosedStateMachine.States.INIT, ".*close.*", Parameter.This, FileMustBeClosedStateMachine.States.CLOSED, Type.OnCall))
  addTransition(new Nothing(FileMustBeClosedStateMachine.States.OPENED, ".*close.*", Parameter.This, FileMustBeClosedStateMachine.States.CLOSED, Type.OnCall))

  @Override def initialState: Nothing = FileMustBeClosedStateMachine.States.INIT

  @Override def generateSeed(unit: Nothing): Nothing = {
    try return generateAtAllocationSiteOf(unit, Class.forName("typestate.test.helper.File"))
    catch {
      case e: Nothing =>
        e.printStackTrace
    }
    Collections.emptySet
  }
}