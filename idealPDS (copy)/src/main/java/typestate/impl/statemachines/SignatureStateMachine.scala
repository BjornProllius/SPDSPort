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
import boomerang.scene.Statement
import java.util
import java.util.Collections
import soot.SootClass
import soot.SootMethod
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type
import typestate.finiteautomata.State
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object SignatureStateMachine {
  private val INIT_SIGN = "initSign"
  private val INIT_VERIFY = "initVerify"
  private val SIGN = "sign"
  private val UPDATE = "update"
  private val VERIFY = "verify"
  private val GET_INSTANCE = "getInstance"

  final class States extends Nothing {}
}

class SignatureStateMachine extends Nothing {
  addTransition(new Nothing(SignatureStateMachine.States.NONE, SignatureStateMachine.GET_INSTANCE, Parameter.This, SignatureStateMachine.States.UNITIALIZED, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.UNITIALIZED, SignatureStateMachine.INIT_SIGN, Parameter.This, SignatureStateMachine.States.SIGN_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.UNITIALIZED, SignatureStateMachine.INIT_VERIFY, Parameter.This, SignatureStateMachine.States.VERIFY_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.UNITIALIZED, SignatureStateMachine.SIGN, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.UNITIALIZED, SignatureStateMachine.VERIFY, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.UNITIALIZED, SignatureStateMachine.UPDATE, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.SIGN_CHECK, SignatureStateMachine.INIT_SIGN, Parameter.This, SignatureStateMachine.States.SIGN_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.SIGN_CHECK, SignatureStateMachine.INIT_VERIFY, Parameter.This, SignatureStateMachine.States.VERIFY_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.SIGN_CHECK, SignatureStateMachine.SIGN, Parameter.This, SignatureStateMachine.States.SIGN_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.SIGN_CHECK, SignatureStateMachine.VERIFY, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.SIGN_CHECK, SignatureStateMachine.UPDATE, Parameter.This, SignatureStateMachine.States.SIGN_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.VERIFY_CHECK, SignatureStateMachine.INIT_SIGN, Parameter.This, SignatureStateMachine.States.SIGN_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.VERIFY_CHECK, SignatureStateMachine.INIT_VERIFY, Parameter.This, SignatureStateMachine.States.VERIFY_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.VERIFY_CHECK, SignatureStateMachine.SIGN, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.VERIFY_CHECK, SignatureStateMachine.VERIFY, Parameter.This, SignatureStateMachine.States.VERIFY_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.VERIFY_CHECK, SignatureStateMachine.UPDATE, Parameter.This, SignatureStateMachine.States.VERIFY_CHECK, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.ERROR, SignatureStateMachine.INIT_SIGN, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.ERROR, SignatureStateMachine.INIT_VERIFY, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.ERROR, SignatureStateMachine.SIGN, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.ERROR, SignatureStateMachine.VERIFY, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))
  addTransition(new Nothing(SignatureStateMachine.States.ERROR, SignatureStateMachine.UPDATE, Parameter.This, SignatureStateMachine.States.ERROR, Type.OnCall))

  private def constructor = {
    val subclasses = getSubclassesOf("java.security.Signature")
    val out = new Nothing
    import scala.collection.JavaConversions._
    for (c <- subclasses) {
      import scala.collection.JavaConversions._
      for (m <- c.getMethods) {
        if (m.isPublic && m.getName.equals("getInstance")) out.add(m)
      }
    }
    out
  }

  @Override def generateSeed(edge: Nothing): Nothing = {
    val unit = edge.getStart
    if (unit.containsInvokeExpr) {
      val method = unit.getInvokeExpr.getMethod
      if (method.getName.equals("getInstance") && method.getSubSignature.contains("Signature")) return getLeftSideOf(edge)
    }
    Collections.emptySet
  }

  @Override protected def initialState: Nothing = SignatureStateMachine.States.UNITIALIZED
}