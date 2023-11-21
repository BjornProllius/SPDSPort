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
import boomerang.scene.DeclaredMethod
import boomerang.scene.Statement
import java.util.Collections
import java.util
import soot.SootClass
import soot.SootMethod
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type
import typestate.finiteautomata.State
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object KeyStoreStateMachine {
  private val LOAD_METHOD = ".* load.*"

  final class States extends Nothing {}
}

class KeyStoreStateMachine extends Nothing {
  // addTransition(new MatcherTransition(States.NONE,
  // keyStoreConstructor(),Parameter.This, States.INIT, Type.OnReturn));
  addTransition(new Nothing(KeyStoreStateMachine.States.INIT, KeyStoreStateMachine.LOAD_METHOD, Parameter.This, KeyStoreStateMachine.States.LOADED, Type.OnCallToReturn))
  addTransition(new Nothing(KeyStoreStateMachine.States.LOADED, KeyStoreStateMachine.LOAD_METHOD, true, Parameter.This, KeyStoreStateMachine.States.LOADED, Type.OnCallToReturn))
  addTransition(new Nothing(KeyStoreStateMachine.States.INIT, KeyStoreStateMachine.LOAD_METHOD, true, Parameter.This, KeyStoreStateMachine.States.ERROR, Type.OnCallToReturn))
  addTransition(new Nothing(KeyStoreStateMachine.States.ERROR, KeyStoreStateMachine.LOAD_METHOD, true, Parameter.This, KeyStoreStateMachine.States.ERROR, Type.OnCallToReturn))

  private def keyStoreConstructor = {
    val subclasses = getSubclassesOf("java.security.KeyStore")
    val out = new Nothing
    import scala.collection.JavaConversions._
    for (c <- subclasses) {
      import scala.collection.JavaConversions._
      for (m <- c.getMethods) {
        if (m.getName.equals("getInstance") && m.isStatic) out.add(m)
      }
    }
    out
  }

  @Override def generateSeed(edge: Nothing): Nothing = {
    val unit = edge.getStart
    if (unit.isAssign && unit.containsInvokeExpr) if (isKeyStoreConstructor(unit.getInvokeExpr.getMethod)) return Collections.singleton(new Nothing(edge, new Nothing(unit.getLeftOp, unit, unit.getRightOp), initialTransition))
    Collections.emptySet
  }

  private def isKeyStoreConstructor(method: Nothing) = method.getName.equals("getInstance") && method.getSubSignature.contains("KeyStore")

  @Override protected def initialState: Nothing = KeyStoreStateMachine.States.INIT
}