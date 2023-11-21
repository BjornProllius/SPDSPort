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

object PrintStreamStateMachine {
  final class States extends Nothing {}

  private val CLOSE_METHODS = ".* close.*"
  private val READ_METHODS = ".* (read|flush|write).*"
  private val TYPE = "java.io.PrintStream"
}

class PrintStreamStateMachine extends Nothing {
  addTransition(new Nothing(PrintStreamStateMachine.States.CLOSED, PrintStreamStateMachine.CLOSE_METHODS, Parameter.This, PrintStreamStateMachine.States.CLOSED, Type.OnCall))
  addTransition(new Nothing(PrintStreamStateMachine.States.CLOSED, PrintStreamStateMachine.READ_METHODS, Parameter.This, PrintStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(PrintStreamStateMachine.States.ERROR, PrintStreamStateMachine.READ_METHODS, Parameter.This, PrintStreamStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(PrintStreamStateMachine.States.ERROR, PrintStreamStateMachine.CLOSE_METHODS, Parameter.This, PrintStreamStateMachine.States.ERROR, Type.OnCall))

  //
  //    private Set<SootMethod> readMethods() {
  //        List<SootClass> subclasses = getSubclassesOf("java.io.PrintStream");
  //        Set<SootMethod> closeMethods = closeMethods();
  //        Set<SootMethod> out = new HashSet<>();
  //        for (SootClass c : subclasses) {
  //            for (SootMethod m : c.getMethods())
  //                if (m.isPublic() && !closeMethods.contains(m) && !m.isStatic())
  //                    out.add(m);
  //        }
  //        return out;
  //    }
  @Override def generateSeed(unit: Nothing): Nothing = this.generateThisAtAnyCallSitesOf(unit, PrintStreamStateMachine.TYPE, PrintStreamStateMachine.CLOSE_METHODS)

  @Override protected def initialState: Nothing = PrintStreamStateMachine.States.CLOSED
}