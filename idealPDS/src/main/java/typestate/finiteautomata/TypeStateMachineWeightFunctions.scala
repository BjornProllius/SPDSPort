/**
 * ******************************************************************************
 * Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors: Johannes Spaeth - initial API and implementation
 * ******************************************************************************
 */

 package typestate.finiteautomata

 import boomerang.{WeightedForwardQuery, scene}
 import boomerang.scene.{AllocVal, ControlFlowGraph, Edge, InvokeExpr, Statement, Val}
 import com.google.common.base.Joiner
 import com.google.common.collect.{Collections2, Sets}
 import org.slf4j.{Logger, LoggerFactory}
 import soot.Scene
 import soot.SootClass
 import sync.pds.solver.{WeightFunctions, nodes}
 import typestate.TransitionFunction
 import typestate.finiteautomata.MatcherTransition.{Parameter, Type}
 
 import scala.collection.JavaConverters._
 import scala.collection.immutable.{Collection, Collections}
 
 abstract class TypeStateMachineWeightFunctions
     extends WeightFunctions[Edge, Val, Edge, TransitionFunction] {
   private val LOGGER: Logger = LoggerFactory.getLogger(classOf[TypeStateMachineWeightFunctions])
   var transition: Set[MatcherTransition] = new HashSet[MatcherTransition]
 
   def addTransition(trans: MatcherTransition): Unit = {
     transition.add(trans)
   }
 
   override def getOne: TransitionFunction = TransitionFunction.one()
 
   def pop(curr: nodes.Node[Edge, Val]): TransitionFunction = {
     LOGGER.trace(s"Getting pop weights for $curr which returns to ${curr.succ}")
     getOne
   }
 
   def push(curr: nodes.Node[Edge, Val], succ: nodes.Node[Edge, Val], push: Edge): TransitionFunction = {
     getMatchingTransitions(
       succ.stmt,
       succ.fact,
       push,
       Collections2.filter(
         transition,
         (input: MatcherTransition) =>
           input.getType.equals(Type.OnCall) || input.getType.equals(Type.OnCallOrOnCallToReturn)
       ).asScala,
       Type.OnCall
     )
   }
 
   override def normal(curr: nodes.Node[Edge, Val], succ: nodes.Node[Edge, Val]): TransitionFunction = {
     if (succ.stmt.getTarget.containsInvokeExpr) {
       callToReturn(curr, succ, succ.stmt.getTarget.getInvokeExpr)
     } else {
       getOne
     }
   }
 
   def callToReturn(
       curr: nodes.Node[Edge, Val],
       succ: nodes.Node[Edge, Val],
       invokeExpr: InvokeExpr
   ): TransitionFunction = {
     val res: Set[Transition] = Sets.newHashSet()
     if (invokeExpr.isInstanceInvokeExpr) {
       if (invokeExpr.getBase.equals(succ.fact)) {
         for (trans <- transition.asScala) {
           if (trans.matches(invokeExpr.getMethod)
               && (trans.getType.equals(Type.OnCallToReturn)
                 || trans.getType.equals(Type.OnCallOrOnCallToReturn))) {
             res.add(trans)
           }
         }
       }
     }
     if (!res.isEmpty) {
       LOGGER.trace(
         s"Typestate transition at ${succ.stmt} to $res, [${Type.OnCallToReturn}]"
       )
     }
     if (res.isEmpty) getOne
     else new TransitionFunction(res.asJava, Collections.singleton(succ.stmt))
   }
 
   private def getMatchingTransitions(
       edge: Edge,
       node: Val,
       transitionEdge: Edge,
       filteredTrans: Collection[MatcherTransition],
       `type`: Type
   ): TransitionFunction = {
     val transitionStmt: Statement = transitionEdge.getStart
     val res: Set[ITransition] = new HashSet[ITransition]
     if (filteredTrans.isEmpty || !transitionStmt.containsInvokeExpr) return getOne
     for (trans <- filteredTrans.asScala) {
       if (trans.matches(transitionStmt.getInvokeExpr.getMethod)) {
         LOGGER.trace(
           s"Found potential transition at $transitionStmt, now checking if parameter match"
         )
         val param: Parameter = trans.getParam
         if (param.equals(Parameter.This) && edge.getMethod.isThisLocal(node)) {
           res.add(new Transition(trans.from, trans.to))
         }
         if (param.equals(Parameter.Param1) && edge.getMethod.getParameterLocal(0).equals(node)) {
           res.add(new Transition(trans.from, trans.to))
         }
         if (param.equals(Parameter.Param2) && edge.getMethod.getParameterLocal(1).equals(node)) {
           res.add(new Transition(trans.from, trans.to))
         }
       }
     }
 
     if (res.isEmpty) return getOne
 
     LOGGER.debug(
       s"Typestate transition at $transitionStmt to $res, [$$type]"
     )
     new TransitionFunction(res.asJava, Collections.singleton(transitionEdge))
   }
 
   protected def getSubclassesOf(className: String): List[SootClass] = {
     val sootClass: SootClass = Scene.v().getSootClass(className)
     val list: List[SootClass] =
       Scene.v().getActiveHierarchy.getSubclassesOfIncluding(sootClass).asScala.toList
     val res: List[SootClass] = List()
     for (c <- list) {
       res.add(c)
     }
     res
   }
 
   protected def getLeftSideOf(edge: Edge): Collection[WeightedForwardQuery[TransitionFunction]] = {
     val s: Statement = edge.getStart
     if (s.isAssign) {
       return Collections.singleton(
         new WeightedForwardQuery[TransitionFunction](
           edge,
           new AllocVal(s.getLeftOp, s, s.getRightOp),
           initialTransition()
         )
       )
     }
     Collections.emptySet()
   }
 
   protected def generateAtAllocationSiteOf(
       edge: Edge,
       allocationSuperType: Class[_]
   ): Collection[WeightedForwardQuery[TransitionFunction]] = {
     val s: Statement = edge.getStart
     if (s.isAssign) {
       if (s.getRightOp.isNewExpr) {
         val newExprType: boomerang.scene.Type = s.getRightOp.getNewExprType
         if (newExprType.isSubtypeOf(allocationSuperType.getName)) {
           return Collections.singleton(
             new WeightedForwardQuery[TransitionFunction](
               edge,
               new AllocVal(s.getLeftOp, s, s.getRightOp),
               initialTransition()
             )
           )
         }
       }
     }
     Collections.emptySet()
   }
 
   def generateThisAtAnyCallSitesOf(
       edge: Edge,
       declaredType: String,
       declaredMethod: String
   ): Collection[WeightedForwardQuery[TransitionFunction]] = {
     val unit: Statement = edge.getStart
     if (unit.containsInvokeExpr) {
       if (unit.getInvokeExpr.isInstanceInvokeExpr) {
         val base: Val = unit.getInvokeExpr.getBase
         if (unit.getInvokeExpr.getMethod.getSignature.matches(declaredMethod)) {
           if (base.getType.isSubtypeOf(declaredType)) {
             return Collections.singleton(
               new WeightedForwardQuery[TransitionFunction](
                 edge,
                 new AllocVal(base, unit, base),
                 initialTransition()
               )
             )
           }
         }
       }
     }
     Collections.emptySet()
   }
 
   override def toString: String = Joiner.on("\n").join(transition.asJava)
 
   def generateSeed(stmt: Edge): Collection[WeightedForwardQuery[TransitionFunction]]
 
   def initialTransition(): TransitionFunction = {
     new TransitionFunction(
       new Transition(initialState(), initialState()),
       Collections.emptySet()
     )
   }
 
   protected def initialState(): State
 }
 