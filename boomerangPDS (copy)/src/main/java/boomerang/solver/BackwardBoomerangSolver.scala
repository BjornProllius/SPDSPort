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
package boomerang.solver

import boomerang.BackwardQuery
import boomerang.BoomerangOptions
import boomerang.callgraph.CalleeListener
import boomerang.callgraph.ObservableICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.controlflowgraph.PredecessorListener
import boomerang.controlflowgraph.SuccessorListener
import boomerang.flowfunction.IBackwardFlowFunction
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Field
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Type
import boomerang.scene.Val
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util.AbstractMap.SimpleEntry
import java.util
import java.util.Map.Entry
import java.util.stream.Collectors
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.PopNode
import sync.pds.solver.nodes.PushNode
import sync.pds.solver.nodes.SingleNode
import wpds.impl.NestedWeightedPAutomatons
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.interfaces.Location
import wpds.interfaces.State

object BackwardBoomerangSolver {
  private val LOGGER = LoggerFactory.getLogger(classOf[BackwardBoomerangSolver[_ <: Nothing]])
}

abstract class BackwardBoomerangSolver[W <: Weight](icfg: Nothing, cfg: Nothing, genField: Nothing, private val query: Nothing, options: Nothing, callSummaries: Nothing, fieldSummaries: Nothing, scope: Nothing, private val flowFunction: Nothing, fieldLoadStatements: Nothing, fieldStoreStatements: Nothing, propagationType: Nothing) extends Nothing(icfg, cfg, genField, options, callSummaries, fieldSummaries, scope, propagationType) {
  this.flowFunction.setSolver(this, fieldLoadStatements, fieldStoreStatements)

  private def notUsedInMethod(m: Nothing, curr: Nothing, value: Nothing): Boolean = {
    if (value.isStatic) return false
    if (!m.getLocals.contains(value)) return true
    false
  }

  def generateFieldState(d: Nothing, loc: Nothing): Nothing = {
    val e = new Nothing(d, loc)
    if (!generatedFieldState.containsKey(e)) generatedFieldState.put(e, new Nothing(new Nothing(new Nothing(epsilonStmt, Val.zero)), loc))
    generatedFieldState.get(e)
  }

  /*
    @Override
    public INode<Val> generateCallState(INode<Val> d, Statement loc) {
      Entry<INode<Val>, Statement> e = new AbstractMap.SimpleEntry<>(d, loc);
      if (!generatedCallState.containsKey(e)) {
        generatedCallState.put(
            e, new GeneratedState<Val, Statement>(new SingleNode<Val>(Val.zero()), loc));
      }
      return generatedCallState.get(e);
    }
    */
  @Override protected def computeReturnFlow(method: Nothing, callerReturnStatement: Nothing, value: Nothing): Nothing = flowFunction.returnFlow(method, callerReturnStatement, value).stream.map((x) => new Nothing(x, PDSSystem.CALLS)).collect(Collectors.toSet)

  protected def callFlow(caller: Nothing, curr: Nothing, callSite: Nothing): Unit = {
    icfg.addCalleeListener(new BackwardBoomerangSolver[W]#CallSiteCalleeListener(curr, caller))
    val invokeExpr = callSite.getInvokeExpr
    if (dataFlowScope.isExcluded(invokeExpr.getMethod)) byPassFlowAtCallsite(caller, curr)
  }

  private def byPassFlowAtCallsite(caller: Nothing, curr: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (returnSite <- curr.stmt.getStart.getMethod.getControlFlowGraph.getPredsOf(curr.stmt.getStart)) {
      val res = flowFunction.callToReturnFlow(new Nothing(returnSite, curr.stmt.getStart), curr.fact).stream.collect(Collectors.toSet)
      import scala.collection.JavaConversions._
      for (s <- res) {
        propagate(curr, s)
      }
    }
  }

  @Override def computeSuccessor(node: Nothing): Unit = {
    BackwardBoomerangSolver.LOGGER.trace("BW: Computing successor of {} for {}", node, this)
    val edge = node.stmt
    val value = node.fact
    assert(!value.isInstanceOf[Nothing])
    val method = edge.getStart.getMethod
    if (method == null) return
    if (dataFlowScope.isExcluded(method)) return
    if (notUsedInMethod(method, edge.getStart, value)) return
    if (edge.getStart.containsInvokeExpr && edge.getStart.uses(value) && INTERPROCEDURAL) callFlow(method, node, edge.getStart)
    else if (icfg.isExitStmt(edge.getStart)) returnFlow(method, node)
    else normalFlow(method, node)
  }

  protected def normalFlow(method: Nothing, currNode: Nothing): Unit = {
    val curr = currNode.stmt
    val value = currNode.fact
    import scala.collection.JavaConversions._
    for (pred <- curr.getStart.getMethod.getControlFlowGraph.getPredsOf(curr.getStart)) {
      val flow = computeNormalFlow(method, new Nothing(pred, curr.getStart), value)
      import scala.collection.JavaConversions._
      for (s <- flow) {
        propagate(currNode, s)
      }
    }
  }

  protected def computeCallFlow(callSiteEdge: Nothing, fact: Nothing, callee: Nothing, calleeStartEdge: Nothing): Nothing = {
    val calleeSp = calleeStartEdge.getTarget
    flowFunction.callFlow(callSiteEdge.getTarget, fact, callee, calleeSp).stream.map((x) => new Nothing(calleeStartEdge, x, callSiteEdge, PDSSystem.CALLS)).collect(Collectors.toSet)
  }

  @Override def processPush(curr: Nothing, location: Nothing, succ: Nothing, system: Nothing): Unit = {
    if (PDSSystem.CALLS eq system) if (!(succ.asInstanceOf[Nothing]).location.getTarget.equals(curr.stmt.getStart) || !curr.stmt.getStart.containsInvokeExpr) throw new Nothing("Invalid push rule")
    super.processPush(curr, location, succ, system)
  }

  @Override protected def computeNormalFlow(method: Nothing, currEdge: Nothing, fact: Nothing): Nothing = flowFunction.normalFlow(currEdge, fact).stream.collect(Collectors.toSet)

  @Override def applyCallSummary(callSiteEdge: Nothing, factAtSpInCallee: Nothing, spInCallee: Nothing, exitStmt: Nothing, exitingFact: Nothing): Unit = {
    val out = Sets.newHashSet
    val callSite = callSiteEdge.getTarget
    if (callSite.containsInvokeExpr) {
      if (exitingFact.isThisLocal) if (callSite.getInvokeExpr.isInstanceInvokeExpr) out.add(new Nothing(callSiteEdge, callSite.getInvokeExpr.getBase))
      if (exitingFact.isReturnLocal) if (callSite.isAssign) out.add(new Nothing(callSiteEdge, callSite.getLeftOp))
      var i = 0
      while (i < callSite.getInvokeExpr.getArgs.size) {
        if (exitingFact.isParameterLocal(i)) out.add(new Nothing(callSiteEdge, callSite.getInvokeExpr.getArg(i)))
        i += 1
      }
    }
    import scala.collection.JavaConversions._
    for (xs <- out) {
      addNormalCallFlow(new Nothing(callSiteEdge, exitingFact), xs)
      addNormalFieldFlow(new Nothing(exitStmt, exitingFact), xs)
    }
  }

  @Override protected def propagateUnbalancedToCallSite(callSite: Nothing, transInCallee: Nothing): Unit = {
    val target = transInCallee.getTarget.asInstanceOf[Nothing]
    if (!callSite.containsInvokeExpr) throw new Nothing("Invalid propagate Unbalanced return")
    if (!isMatchingCallSiteCalleePair(callSite, transInCallee.getLabel.getMethod)) return
    cfg.addSuccsOfListener(new Nothing(callSite) {
      @Override def getSuccessor(succ: Nothing): Unit = {
        cfg.addPredsOfListener(new Nothing(callSite) {
          @Override def getPredecessor(pred: Nothing): Unit = {
            val curr = new Nothing(new Nothing(callSite, succ), query.`var`)
            val callTrans = new Nothing(wrap(curr.fact), curr.stmt, generateCallState(wrap(curr.fact), curr.stmt))
            callAutomaton.addTransition(callTrans)
            callAutomaton.addUnbalancedState(generateCallState(wrap(curr.fact), curr.stmt), target)
            val s = new Nothing(target.location, target.node.fact, new Nothing(pred, callSite), PDSSystem.CALLS)
            propagate(curr, s)
          }
        })
      }
    })
  }

  final private class CallSiteCalleeListener private(private val curr: Nothing, private val caller: Nothing) extends Nothing {
    this.callSite = strongUpdateNode.stmt.getStart
    final private var callSite: Nothing = null

    @Override def getObservedCaller: Nothing = callSite

    @Override def onCalleeAdded(callSite: Nothing, callee: Nothing): Unit = {
      if (callee.isStaticInitializer) return
      import scala.collection.JavaConversions._
      for (calleeSp <- icfg.getStartPointsOf(callee)) {
        import scala.collection.JavaConversions._
        for (predOfCall <- callSite.getMethod.getControlFlowGraph.getPredsOf(callSite)) {
          val res = computeCallFlow(new Nothing(predOfCall, callSite), curr.fact, callee, new Nothing(calleeSp, calleeSp))
          import scala.collection.JavaConversions._
          for (o <- res) {
            thisBackwardBoomerangSolver.propagate(curr, o)
          }
        }
      }
    }

    @Override def onNoCalleeFound(): Unit = {
      byPassFlowAtCallsite(caller, curr)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (caller == null) 0
      else caller.hashCode)
      result = prime * result + (if (curr == null) 0
      else curr.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[BackwardBoomerangSolver[W]#CallSiteCalleeListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (caller == null) if (other.caller != null) return false
      else if (!caller.equals(other.caller)) return false
      if (curr == null) if (other.curr != null) return false
      else if (!curr.equals(other.curr)) return false
      true
    }

    private def getOuterType = thisBackwardBoomerangSolver
  }

  @Override def toString: Nothing = "BackwardBoomerangSolver{" + "query=" + query + '}'
}