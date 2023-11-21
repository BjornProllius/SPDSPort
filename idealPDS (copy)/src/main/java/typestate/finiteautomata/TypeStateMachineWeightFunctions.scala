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
package typestate.finiteautomata

import boomerang.WeightedForwardQuery
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.InvokeExpr
import boomerang.scene.Statement
import boomerang.scene.Val
import com.google.common.base.Joiner
import com.google.common.collect.Collections2
import com.google.common.collect.Sets
import java.util
import java.util.Collections
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import soot.Scene
import soot.SootClass
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node
import typestate.TransitionFunction
import typestate.finiteautomata.MatcherTransition.Parameter
import typestate.finiteautomata.MatcherTransition.Type

object TypeStateMachineWeightFunctions {
  private val LOGGER = LoggerFactory.getLogger(classOf[TypeStateMachineWeightFunctions])
}

abstract class TypeStateMachineWeightFunctions extends Nothing {
  var transition = new Nothing

  def addTransition(trans: Nothing): Unit = {
    transition.add(trans)
  }

  @Override def getOne: Nothing = TransitionFunction.one

  def pop(curr: Nothing): Nothing = {
    TypeStateMachineWeightFunctions.LOGGER.trace("Getting pop weights for {} which returns to {}", curr)
    getOne
  }

  def push(curr: Nothing, succ: Nothing, push: Nothing): Nothing = getMatchingTransitions(succ.stmt, succ.fact, push, Collections2.filter(transition, (input) => input.getType.equals(Type.OnCall) || input.getType.equals(Type.OnCallOrOnCallToReturn)), Type.OnCall)

  @Override def normal(curr: Nothing, succ: Nothing): Nothing = {
    if (succ.stmt.getTarget.containsInvokeExpr) return callToReturn(curr, succ, succ.stmt.getTarget.getInvokeExpr)
    getOne
  }

  def callToReturn(curr: Nothing, succ: Nothing, invokeExpr: Nothing): Nothing = {
    val res = Sets.newHashSet
    if (invokeExpr.isInstanceInvokeExpr) if (invokeExpr.getBase.equals(succ.fact)) {
      import scala.collection.JavaConversions._
      for (trans <- transition) {
        if (trans.matches(invokeExpr.getMethod) && (trans.getType.equals(Type.OnCallToReturn) || trans.getType.equals(Type.OnCallOrOnCallToReturn))) res.add(trans)
      }
    }
    if (!res.isEmpty) TypeStateMachineWeightFunctions.LOGGER.trace("Typestate transition at {} to {}, [{}]", succ.stmt, res, Type.OnCallToReturn)
    if (res.isEmpty) getOne
    else new Nothing(res, Collections.singleton(succ.stmt))
  }

  private def getMatchingTransitions(edge: Nothing, node: Nothing, transitionEdge: Nothing, filteredTrans: Nothing, `type`: Nothing): Nothing = {
    val transitionStmt = transitionEdge.getStart
    val res = new Nothing
    if (filteredTrans.isEmpty || !transitionStmt.containsInvokeExpr) return getOne
    import scala.collection.JavaConversions._
    for (trans <- filteredTrans) {
      if (trans.matches(transitionStmt.getInvokeExpr.getMethod)) {
        TypeStateMachineWeightFunctions.LOGGER.trace("Found potential transition at {}, now checking if parameter match", transitionStmt)
        val param = trans.getParam
        if (param.equals(Parameter.This) && edge.getMethod.isThisLocal(node)) res.add(new Nothing(trans.from, trans.to))
        if (param.equals(Parameter.Param1) && edge.getMethod.getParameterLocal(0).equals(node)) res.add(new Nothing(trans.from, trans.to))
        if (param.equals(Parameter.Param2) && edge.getMethod.getParameterLocal(1).equals(node)) res.add(new Nothing(trans.from, trans.to))
      }
    }
    if (res.isEmpty) return getOne
    TypeStateMachineWeightFunctions.LOGGER.debug("Typestate transition at {} to {}, [{}]", transitionStmt, res, `type`)
    new Nothing(res, Collections.singleton(transitionEdge))
  }

  protected def getSubclassesOf(className: Nothing): Nothing = {
    val sootClass = Scene.v.getSootClass(className)
    val list = Scene.v.getActiveHierarchy.getSubclassesOfIncluding(sootClass)
    val res = new Nothing
    import scala.collection.JavaConversions._
    for (c <- list) {
      res.add(c)
    }
    res
  }

  protected def getLeftSideOf(edge: Nothing): Nothing = {
    val s = edge.getStart
    if (s.isAssign) return Collections.singleton(new Nothing(edge, new Nothing(s.getLeftOp, s, s.getRightOp), initialTransition))
    Collections.emptySet
  }

  protected def generateAtAllocationSiteOf(edge: Nothing, allocationSuperType: Nothing): Nothing = {
    val s = edge.getStart
    if (s.isAssign) if (s.getRightOp.isNewExpr) {
      val newExprType = s.getRightOp.getNewExprType
      if (newExprType.isSubtypeOf(allocationSuperType.getName)) return Collections.singleton(new Nothing(edge, new Nothing(s.getLeftOp, s, s.getRightOp), initialTransition))
    }
    Collections.emptySet
  }

  def generateThisAtAnyCallSitesOf(edge: Nothing, declaredType: Nothing, declaredMethod: Nothing): Nothing = {
    val unit = edge.getStart
    if (unit.containsInvokeExpr) if (unit.getInvokeExpr.isInstanceInvokeExpr) {
      val base = unit.getInvokeExpr.getBase
      if (unit.getInvokeExpr.getMethod.getSignature.matches(declaredMethod)) if (base.getType.isSubtypeOf(declaredType)) return Collections.singleton(new Nothing(edge, new Nothing(base, unit, base), initialTransition))
    }
    Collections.emptySet
  }

  @Override def toString: Nothing = Joiner.on("\n").join(transition)

  def generateSeed(stmt: Nothing): Nothing

  def initialTransition = new Nothing(new Nothing(initialState, initialState), Collections.emptySet)

  protected def initialState: Nothing
}