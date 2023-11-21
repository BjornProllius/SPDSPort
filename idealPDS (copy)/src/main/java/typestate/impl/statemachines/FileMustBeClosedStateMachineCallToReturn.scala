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

object FileMustBeClosedStateMachineCallToReturn {
  final class States extends Nothing {}
}

class FileMustBeClosedStateMachineCallToReturn extends Nothing {
  addTransition(new Nothing(FileMustBeClosedStateMachineCallToReturn.States.INIT, ".*open.*", Parameter.This, FileMustBeClosedStateMachineCallToReturn.States.OPENED, Type.OnCallToReturn))
  addTransition(new Nothing(FileMustBeClosedStateMachineCallToReturn.States.INIT, ".*close.*", Parameter.This, FileMustBeClosedStateMachineCallToReturn.States.CLOSED, Type.OnCallToReturn))
  addTransition(new Nothing(FileMustBeClosedStateMachineCallToReturn.States.OPENED, ".*close.*", Parameter.This, FileMustBeClosedStateMachineCallToReturn.States.CLOSED, Type.OnCallToReturn))

  @Override def initialState: Nothing = FileMustBeClosedStateMachineCallToReturn.States.INIT

  @Override def generateSeed(edge: Nothing): Nothing = {
    try return generateAtAllocationSiteOf(edge, Class.forName("typestate.test.helper.File"))
    catch {
      case e: Nothing =>
        e.printStackTrace
    }
    Collections.emptySet
  }
}