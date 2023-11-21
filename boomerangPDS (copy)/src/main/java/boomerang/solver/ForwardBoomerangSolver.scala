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

import boomerang.BoomerangOptions
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.callgraph.CalleeListener
import boomerang.callgraph.ObservableICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.controlflowgraph.PredecessorListener
import boomerang.controlflowgraph.SuccessorListener
import boomerang.flowfunction.IForwardFlowFunction
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
import java.util
import java.util.Collections
import java.util.Map.Entry
import java.util.stream.Collectors
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
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.WPAStateListener

object ForwardBoomerangSolver {
  private val LOGGER = LoggerFactory.getLogger(classOf[ForwardBoomerangSolver[_ <: Nothing]])
}

abstract class ForwardBoomerangSolver[W <: Weight](callGraph: Nothing, cfg: Nothing, private val query: Nothing, genField: Nothing, options: Nothing, callSummaries: Nothing, fieldSummaries: Nothing, scope: Nothing, private val flowFunctions: Nothing, fieldLoadStatements: Nothing, fieldStoreStatements: Nothing, propagationType: Nothing) extends Nothing(callGraph, cfg, genField, options, callSummaries, fieldSummaries, scope, propagationType) {
  this.flowFunctions.setSolver(this, fieldLoadStatements, fieldStoreStatements)

  @Override def processPush(curr: Nothing, location: Nothing, succ: Nothing, system: Nothing): Unit = {
    if (PDSSystem.CALLS eq system) if (!(succ.asInstanceOf[Nothing]).location.getStart.equals(curr.stmt.getTarget) || !curr.stmt.getTarget.containsInvokeExpr) throw new Nothing("Invalid push rule")
    super.processPush(curr, location, succ, system)
  }

  final private class OverwriteAtFieldStore private(state: Nothing, private val nextStmt: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(nextStmt.getTarget.getFieldStore.getY)) {
        ForwardBoomerangSolver.LOGGER.trace("Overwriting field {} at {}", t.getLabel, nextStmt)
        overwriteFieldAtStatement(nextStmt, t)
      }
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (nextStmt == null) 0
      else nextStmt.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ForwardBoomerangSolver[W]#OverwriteAtFieldStore]
      if (!getEnclosingInstance.equals(other.getEnclosingInstance)) return false
      if (nextStmt == null) if (other.nextStmt != null) return false
      else if (!nextStmt.equals(other.nextStmt)) return false
      true
    }

    private def getEnclosingInstance = thisForwardBoomerangSolver
  }

  final private class OverwriteAtArrayStore private(state: Nothing, private val nextStmt: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(Field.array(nextStmt.getTarget.getArrayBase.getY))) {
        ForwardBoomerangSolver.LOGGER.trace("Overwriting field {} at {}", t.getLabel, nextStmt)
        overwriteFieldAtStatement(nextStmt, t)
      }
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (nextStmt == null) 0
      else nextStmt.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ForwardBoomerangSolver[W]#OverwriteAtArrayStore]
      if (!getEnclosingInstance.equals(other.getEnclosingInstance)) return false
      if (nextStmt == null) if (other.nextStmt != null) return false
      else if (!nextStmt.equals(other.nextStmt)) return false
      true
    }

    private def getEnclosingInstance = thisForwardBoomerangSolver
  }

  @Override protected def propagateUnbalancedToCallSite(callSite: Nothing, transInCallee: Nothing): Unit = {
    val target = transInCallee.getTarget.asInstanceOf[Nothing]
    if (!callSite.containsInvokeExpr) throw new Nothing("Invalid propagate Unbalanced return")
    if (!isMatchingCallSiteCalleePair(callSite, transInCallee.getLabel.getMethod)) return
    cfg.addSuccsOfListener(new Nothing(callSite) {
      @Override def getSuccessor(succ: Nothing): Unit = {
        cfg.addPredsOfListener(new Nothing(callSite) {
          @Override def getPredecessor(pred: Nothing): Unit = {
            val curr = new Nothing(new Nothing(pred, callSite), query.`var`)
            /**
             * Transition<Field, INode<Node<Statement, Val>>> fieldTrans = new
             * Transition<>(new SingleNode<>(curr), emptyField(), new SingleNode<>(curr));
             * fieldAutomaton.addTransition(fieldTrans);*
             */
            val callTrans = new Nothing(wrap(curr.fact), curr.stmt, generateCallState(wrap(curr.fact), curr.stmt))
            callAutomaton.addTransition(callTrans)
            callAutomaton.addUnbalancedState(generateCallState(wrap(curr.fact), curr.stmt), target)
            val s = new Nothing(target.location, target.node.fact, new Nothing(callSite, succ), PDSSystem.CALLS)
            propagate(curr, s)
          }
        })
      }
    })
  }

  final private class CallSiteCalleeListener private(private val caller: Nothing, private val callSiteEdge: Nothing, private val currNode: Nothing, private val invokeExpr: Nothing) extends Nothing {
    this.callSite = callSiteEdge.getStart
    final private var callSite: Nothing = null

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (callSiteEdge == null) 0
      else callSiteEdge.hashCode)
      result = prime * result + (if (caller == null) 0
      else caller.hashCode)
      result = prime * result + (if (currNode == null) 0
      else currNode.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ForwardBoomerangSolver[W]#CallSiteCalleeListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (callSiteEdge == null) if (other.callSiteEdge != null) return false
      else if (!callSiteEdge.equals(other.callSiteEdge)) return false
      if (caller == null) if (other.caller != null) return false
      else if (!caller.equals(other.caller)) return false
      if (currNode == null) if (other.currNode != null) return false
      else if (!currNode.equals(other.currNode)) return false
      true
    }

    @Override def onCalleeAdded(callSite: Nothing, callee: Nothing): Unit = {
      if (callee.isStaticInitializer) return
      ForwardBoomerangSolver.LOGGER.trace("Call-flow of {} at callsite: {} to callee method: {} for {}", currNode.fact, callSite, callee, this)
      import scala.collection.JavaConversions._
      for (calleeSp <- icfg.getStartPointsOf(callee)) {
        val res = computeCallFlow(caller, callSite, callSiteEdge, currNode, callee, new Nothing(calleeSp, calleeSp))
        import scala.collection.JavaConversions._
        for (s <- res) {
          propagate(currNode, s)
        }
      }
    }

    @Override def onNoCalleeFound(): Unit = {
      byPassFlowAtCallSite(caller, currNode, callSite)
    }

    @Override def getObservedCaller: Nothing = callSite

    private def getOuterType = thisForwardBoomerangSolver
  }

  @Override def applyCallSummary(returnSiteStatement: Nothing, factInCallee: Nothing, spInCallee: Nothing, lastCfgEdgeInCallee: Nothing, returnedFact: Nothing): Unit = {
    val callSite = returnSiteStatement.getStart
    val out = Sets.newHashSet
    if (callSite.containsInvokeExpr) {
      if (returnedFact.isThisLocal) if (callSite.getInvokeExpr.isInstanceInvokeExpr) out.add(new Nothing(returnSiteStatement, callSite.getInvokeExpr.getBase))
      if (returnedFact.isReturnLocal) if (callSite.isAssign) out.add(new Nothing(returnSiteStatement, callSite.getLeftOp))
      var i = 0
      while (i < callSite.getInvokeExpr.getArgs.size) {
        if (returnedFact.isParameterLocal(i)) out.add(new Nothing(returnSiteStatement, callSite.getInvokeExpr.getArg(i)))
        i += 1
      }
    }
    if (returnedFact.isStatic) out.add(new Nothing(returnSiteStatement, returnedFact.withNewMethod(returnSiteStatement.getStart.getMethod)))
    import scala.collection.JavaConversions._
    for (xs <- out) {
      addNormalCallFlow(new Nothing(returnSiteStatement, returnedFact), xs)
      addNormalFieldFlow(new Nothing(lastCfgEdgeInCallee, returnedFact), xs)
    }
  }

  def computeCallFlow(caller: Nothing, callSite: Nothing, succOfCallSite: Nothing, currNode: Nothing, callee: Nothing, calleeStartEdge: Nothing): Nothing = {
    if (dataFlowScope.isExcluded(callee)) {
      byPassFlowAtCallSite(caller, currNode, callSite)
      return Collections.emptySet
    }
    val fact = currNode.fact
    flowFunctions.callFlow(callSite, fact, callee).stream.map((x) => new Nothing(calleeStartEdge, x, succOfCallSite, PDSSystem.CALLS)).collect(Collectors.toSet)
  }

  def getQuery: Nothing = query

  @Override def computeSuccessor(node: Nothing): Unit = {
    val curr = node.stmt
    val value = node.fact
    assert(!value.isInstanceOf[Nothing])
    val method = curr.getTarget.getMethod
    if (method == null) return
    if (dataFlowScope.isExcluded(method)) return
    if (icfg.isExitStmt(curr.getTarget)) {
      returnFlow(method, node)
      return
    }
    cfg.addSuccsOfListener(new Nothing(curr.getTarget) {
      @Override def getSuccessor(succ: Nothing): Unit = {
        if (query.getType.isNullType && curr.getStart.isIfStmt && curr.getStart.killAtIfStmt(value, succ)) return
        if (!method.getLocals.contains(value) && !value.isStatic) return
        if (curr.getTarget.containsInvokeExpr && (curr.getTarget.isParameter(value) || value.isStatic)) callFlow(method, node, new Nothing(curr.getTarget, succ), curr.getTarget.getInvokeExpr)
        else {
          checkForFieldOverwrite(curr, value)
          val out = computeNormalFlow(method, new Nothing(curr.getTarget, succ), value)
          import scala.collection.JavaConversions._
          for (s <- out) {
            ForwardBoomerangSolver.LOGGER.trace("{}: {} -> {}", s, node, thisForwardBoomerangSolver.query)
            propagate(node, s)
          }
        }
      }
    })
  }

  private def checkForFieldOverwrite(curr: Nothing, value: Nothing): Unit = {
    if (curr.getTarget.isFieldStore && curr.getTarget.getFieldStore.getX.equals(value)) {
      val node = new Nothing(curr, value)
      fieldAutomaton.registerListener(new ForwardBoomerangSolver[W]#OverwriteAtFieldStore(new Nothing(node), curr))
    }
    else if (curr.getTarget.isArrayStore && curr.getTarget.getArrayBase.getX.equals(value)) {
      val node = new Nothing(curr, value)
      fieldAutomaton.registerListener(new ForwardBoomerangSolver[W]#OverwriteAtArrayStore(new Nothing(node), curr))
    }
  }

  protected def overwriteFieldAtStatement(fieldWriteStatementEdge: Nothing, killedTransition: Nothing): Unit

  @Override def computeNormalFlow(method: Nothing, nextEdge: Nothing, fact: Nothing): Nothing = flowFunctions.normalFlow(query, nextEdge, fact)

  protected def callFlow(caller: Nothing, currNode: Nothing, callSiteEdge: Nothing, invokeExpr: Nothing): Unit = {
    assert(icfg.isCallStmt(callSiteEdge.getStart))
    if (dataFlowScope.isExcluded(invokeExpr.getMethod)) byPassFlowAtCallSite(caller, currNode, callSiteEdge.getStart)
    icfg.addCalleeListener(new ForwardBoomerangSolver[W]#CallSiteCalleeListener(caller, callSiteEdge, currNode, invokeExpr))
  }

  private def byPassFlowAtCallSite(caller: Nothing, currNode: Nothing, callSite: Nothing): Unit = {
    ForwardBoomerangSolver.LOGGER.trace("Bypassing call flow of {} at callsite: {} for {}", currNode.fact, callSite, this)
    cfg.addSuccsOfListener(new Nothing(currNode.stmt.getTarget) {
      @Override def getSuccessor(returnSite: Nothing): Unit = {
        import scala.collection.JavaConversions._
        for (s <- flowFunctions.callToReturnFlow(query, new Nothing(callSite, returnSite), currNode.fact)) {
          propagate(currNode, s)
        }
      }
    })
  }

  @Override def computeReturnFlow(method: Nothing, curr: Nothing, value: Nothing): Nothing = flowFunctions.returnFlow(method, curr, value).stream.map((x) => new Nothing(x, PDSSystem.CALLS)).collect(Collectors.toSet)

  @Override def toString: Nothing = "ForwardSolver: " + query
}