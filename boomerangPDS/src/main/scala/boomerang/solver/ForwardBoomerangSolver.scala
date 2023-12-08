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

import boomerang.{BoomerangOptions, ForwardQuery, Query}
import boomerang.callgraph.{CalleeListener, ObservableICFG}
import boomerang.controlflowgraph.{ObservableControlFlowGraph, PredecessorListener, SuccessorListener}
import boomerang.flowfunction.IForwardFlowFunction
import boomerang.scene.{AllocVal, ControlFlowGraph, Field, InvokeExpr, Method, Statement, Type, Val}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import com.google.common.collect.{Multimap, Sets}
import java.util.{Collection, Collections, Map, Set}
import java.util.stream.Collectors
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.{GeneratedState, INode, Node, PopNode, PushNode, SingleNode}
import wpds.impl.{NestedWeightedPAutomatons, Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.{Location, State, WPAStateListener}

abstract class ForwardBoomerangSolver[W <: Weight] (
  callGraph: ObservableICFG[Statement, Method],
  cfg: ObservableControlFlowGraph,
  query: ForwardQuery,
  genField: Map[Entry[INode[Node[Edge, Val]], Field], INode[Node[Edge, Val]]],
  options: BoomerangOptions,
  callSummaries: NestedWeightedPAutomatons[Edge, INode[Val], W],
  fieldSummaries: NestedWeightedPAutomatons[Field, INode[Node[Edge, Val]], W],
  scope: DataFlowScope,
  flowFunctions: IForwardFlowFunction,
  fieldLoadStatements: Multimap[Field, Statement],
  fieldStoreStatements: Multimap[Field, Statement],
  propagationType: Type
  ) extends AbstractBoomerangSolver[W](
  callGraph, cfg, genField, options, callSummaries, fieldSummaries, scope, propagationType
  ) {
  private val LOGGER = LoggerFactory.getLogger(classOf[ForwardBoomerangSolver[_]])

  flowFunctions.setSolver(this, fieldLoadStatements, fieldStoreStatements)

  override def processPush(
    curr: Node[Edge, Val], location: Location, succ: PushNode[Edge, Val, _], system: PDSSystem
  ): Unit = {
    if (PDSSystem.CALLS == system) {
      if (!succ.location().getStart().equals(curr.stmt().getTarget())
          || !curr.stmt().getTarget().containsInvokeExpr()) {
        throw new RuntimeException("Invalid push rule")
      }
    }
    super.processPush(curr, location, succ, system)
  }

  private final class OverwriteAtFieldStore(state: INode[Node[Edge, Val]], nextEdge: Edge)
    extends WPAStateListener[Field, INode[Node[ControlFlowGraph.Edge, Val]], W](state) {

    private val nextStmt = nextEdge

    override def onOutTransitionAdded(
      t: Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]],
      w: W,
      weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
    ): Unit = {
      if (t.getLabel().equals(nextStmt.getTarget().getFieldStore().getY())) {
        LOGGER.trace("Overwriting field {} at {}", t.getLabel(), nextStmt)
        overwriteFieldAtStatement(nextStmt, t)
      }
    }

    override def onInTransitionAdded(
      t: Transition[Field, INode[Node[ControlFlowGraph.Edge, Val]]],
      w: W,
      weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
    ): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + getEnclosingInstance().hashCode()
      result = prime * result + (if (nextStmt == null) 0 else nextStmt.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: OverwriteAtFieldStore =>
          super.equals(that) &&
          getEnclosingInstance().equals(that.getEnclosingInstance()) &&
          (if (nextStmt == null) that.nextStmt == null else nextStmt.equals(that.nextStmt))
        case _ => false
      }
    }

    private def getEnclosingInstance(): ForwardBoomerangSolver[W] = ForwardBoomerangSolver.this
  }

  private final class OverwriteAtArrayStore(state: INode[Node[Edge, Val]], nextStmt: Edge)
    extends WPAStateListener[Field, INode[Node[Edge, Val]], W](state) {

    private val nextStmt = nextStmt

    override def onOutTransitionAdded(
      t: Transition[Field, INode[Node[Edge, Val]]],
      w: W,
      weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
    ): Unit = {
      if (t.getLabel().equals(Field.array(nextStmt.getTarget().getArrayBase().getY()))) {
        LOGGER.trace("Overwriting field {} at {}", t.getLabel(), nextStmt)
        overwriteFieldAtStatement(nextStmt, t)
      }
    }

    override def onInTransitionAdded(
      t: Transition[Field, INode[Node[Edge, Val]]],
      w: W,
      weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
    ): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + getEnclosingInstance().hashCode()
      result = prime * result + (if (nextStmt == null) 0 else nextStmt.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: OverwriteAtArrayStore =>
          super.equals(that) &&
          getEnclosingInstance().equals(that.getEnclosingInstance()) &&
          (if (nextStmt == null) that.nextStmt == null else nextStmt.equals(that.nextStmt))
        case _ => false
      }
    }

    private def getEnclosingInstance(): ForwardBoomerangSolver[W] = ForwardBoomerangSolver.this
  }

  override protected def propagateUnbalancedToCallSite(
    callSite: Statement, transInCallee: Transition[ControlFlowGraph.Edge, INode[Val]]
  ): Unit = {
    val target = transInCallee.getTarget().asInstanceOf[GeneratedState[Val, Edge]]
    if (!callSite.containsInvokeExpr()) {
      throw new RuntimeException("Invalid propagate Unbalanced return")
    }
    if (!isMatchingCallSiteCalleePair(callSite, transInCallee.getLabel().getMethod())) {
      return
    }
    cfg.addSuccsOfListener(
      new SuccessorListener(callSite) {
        override def getSuccessor(succ: Statement): Unit = {
          cfg.addPredsOfListener(
            new PredecessorListener(callSite) {
              override def getPredecessor(pred: Statement): Unit = {
                val curr = new Node[ControlFlowGraph.Edge, Val](new Edge(pred, callSite), query.var())
                val callTrans = new Transition[ControlFlowGraph.Edge, INode[Val]](
                  wrap(curr.fact()),
                  curr.stmt(),
                  generateCallState(wrap(curr.fact()), curr.stmt())
                )
                callAutomaton.addTransition(callTrans)
                callAutomaton.addUnbalancedState(
                  generateCallState(wrap(curr.fact()), curr.stmt()), target
                )
                val s = new PushNode[ControlFlowGraph.Edge, Val](
                  target.location(),
                  target.node().fact(),
                  new Edge(callSite, succ),
                  PDSSystem.CALLS
                )
                propagate(curr, s)
              }
            }
          )
        }
      }
    )
  }

  private final class CallSiteCalleeListener(
    caller: Method,
    callSiteEdge: Edge,
    currNode: Node[ControlFlowGraph.Edge, Val],
    invokeExpr: InvokeExpr
  ) extends CalleeListener[Statement, Method] {

    private val callSite = callSiteEdge.getStart()

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType().hashCode()
      result = prime * result + (if (callSiteEdge == null) 0 else callSiteEdge.hashCode())
      result = prime * result + (if (caller == null) 0 else caller.hashCode())
      result = prime * result + (if (currNode == null) 0 else currNode.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: CallSiteCalleeListener =>
          this.eq(that) ||
          (callSiteEdge == that.callSiteEdge &&
          caller == that.caller &&
          currNode == that.currNode &&
          getOuterType().equals(that.getOuterType()))
        case _ => false
      }
    }

    override def onCalleeAdded(callSite: Statement, callee: Method): Unit = {
      if (!callee.isStaticInitializer()) {
        LOGGER.trace(
          "Call-flow of {} at callsite: {} to callee method: {} for {}",
          currNode.fact(),
          callSite,
          callee,
          this
        )
        for (calleeSp <- icfg.getStartPointsOf(callee)) {
          val res = computeCallFlow(
            caller, callSite, callSiteEdge, currNode, callee, new Edge(calleeSp, calleeSp)
          )
          for (s <- res) {
            propagate(currNode, s)
          }
        }
      }
    }

    override def onNoCalleeFound(): Unit = {
      byPassFlowAtCallSite(caller, currNode, callSite)
    }

    override def getObservedCaller(): Statement = {
      callSite
    }

    private def getOuterType(): ForwardBoomerangSolver = {
      ForwardBoomerangSolver.this
    }
  }

  override def applyCallSummary(
    returnSiteStatement: ControlFlowGraph.Edge,
    factInCallee: Val,
    spInCallee: Edge,
    lastCfgEdgeInCallee: Edge,
    returnedFact: Val
  ): Unit = {
    val callSite = returnSiteStatement.getStart()

    val out = Set[Node[ControlFlowGraph.Edge, Val]]()
    if (callSite.containsInvokeExpr()) {
      if (returnedFact.isThisLocal()) {
        if (callSite.getInvokeExpr().isInstanceInvokeExpr()) {
          out.add(new Node(returnSiteStatement, callSite.getInvokeExpr().getBase()))
        }
      }
      if (returnedFact.isReturnLocal()) {
        if (callSite.isAssign()) {
          out.add(new Node(returnSiteStatement, callSite.getLeftOp()))
        }
      }
      for (i <- 0 until callSite.getInvokeExpr().getArgs().size()) {
        if (returnedFact.isParameterLocal(i)) {
          out.add(new Node(returnSiteStatement, callSite.getInvokeExpr().getArg(i)))
        }
      }
    }
    if (returnedFact.isStatic()) {
      out.add(
        new Node(
          returnSiteStatement,
          returnedFact.withNewMethod(returnSiteStatement.getStart().getMethod())
        )
      )
    }
    for (xs <- out) {
      addNormalCallFlow(new Node(returnSiteStatement, returnedFact), xs)
      addNormalFieldFlow(new Node(lastCfgEdgeInCallee, returnedFact), xs)
    }
  }

  def computeCallFlow(
    caller: Method,
    callSite: Statement,
    succOfCallSite: Edge,
    currNode: Node[ControlFlowGraph.Edge, Val],
    callee: Method,
    calleeStartEdge: Edge
  ): Collection[_ <: State] = {
    if (dataFlowScope.isExcluded(callee)) {
      byPassFlowAtCallSite(caller, currNode, callSite)
      return Collections.emptySet()
    }
    val fact = currNode.fact()
    flowFunctions.callFlow(callSite, fact, callee).stream()
      .map(x => new PushNode[Edge, Val](calleeStartEdge, x, succOfCallSite, PDSSystem.CALLS))
      .collect(Collectors.toSet())
  }

  def getQuery(): Query = {
    query
  }

  override def computeSuccessor(node: Node[Edge, Val]): Unit = {
    val curr = node.stmt()
    val value = node.fact()
    assert(!value.isInstanceOf[AllocVal])
    val method = curr.getTarget().getMethod()
    if (method == null) return
    if (dataFlowScope.isExcluded(method)) {
      return
    }
    if (icfg.isExitStmt(curr.getTarget())) {
      returnFlow(method, node)
      return
    }
    cfg.addSuccsOfListener(
      new SuccessorListener(curr.getTarget()) {
        override def getSuccessor(succ: Statement): Unit = {
          if (query.getType().isNullType()
              && curr.getStart().isIfStmt()
              && curr.getStart().killAtIfStmt(value, succ)) {
            return
          }

          if (!method.getLocals().contains(value) && !value.isStatic()) {
            return
          }
          if (curr.getTarget().containsInvokeExpr()
              && (curr.getTarget().isParameter(value) || value.isStatic())) {
            callFlow(
              method, node, new Edge(curr.getTarget(), succ), curr.getTarget().getInvokeExpr())
          } else {
            checkForFieldOverwrite(curr, value)
            val out = computeNormalFlow(method, new Edge(curr.getTarget(), succ), value)
            for (s <- out) {
              LOGGER.trace("{}: {} -> {}", s, node, ForwardBoomerangSolver.this.query)
              propagate(node, s)
            }
          }
        }
      }
    )
  }

  private def checkForFieldOverwrite(curr: Edge, value: Val): Unit = {
    if (curr.getTarget().isFieldStore() && curr.getTarget().getFieldStore().getX().equals(value)) {
      val node = new Node[ControlFlowGraph.Edge, Val](curr, value)
      fieldAutomaton.registerListener(new OverwriteAtFieldStore(new SingleNode[Node[Edge, Val]](node), curr))
    } else if (curr.getTarget().isArrayStore() && curr.getTarget().getArrayBase().getX().equals(value)) {
      val node = new Node[ControlFlowGraph.Edge, Val](curr, value)
      fieldAutomaton.registerListener(new OverwriteAtArrayStore(new SingleNode[Node[Edge, Val]](node), curr))
    }
  }

  protected def overwriteFieldAtStatement(
    fieldWriteStatementEdge: Edge, 
    killedTransition: Transition[Field, INode[Node[Edge, Val]]]
  ): Unit

  override def computeNormalFlow(method: Method, nextEdge: Edge, fact: Val): Collection[State] = {
    flowFunctions.normalFlow(query, nextEdge, fact)
  }

  protected def callFlow(
    caller: Method,
    currNode: Node[ControlFlowGraph.Edge, Val],
    callSiteEdge: Edge,
    invokeExpr: InvokeExpr
  ): Unit = {
    assert(icfg.isCallStmt(callSiteEdge.getStart()))
    if (dataFlowScope.isExcluded(invokeExpr.getMethod())) {
      byPassFlowAtCallSite(caller, currNode, callSiteEdge.getStart())
    }

    icfg.addCalleeListener(new CallSiteCalleeListener(caller, callSiteEdge, currNode, invokeExpr))
  }

  private def byPassFlowAtCallSite(
    caller: Method, 
    currNode: Node[ControlFlowGraph.Edge, Val], 
    callSite: Statement
  ): Unit = {
    LOGGER.trace("Bypassing call flow of {} at callsite: {} for {}", currNode.fact(), callSite, this)

    cfg.addSuccsOfListener(
      new SuccessorListener(currNode.stmt().getTarget()) {
        override def getSuccessor(returnSite: Statement): Unit = {
          for (s <- flowFunctions.callToReturnFlow(query, new Edge(callSite, returnSite), currNode.fact())) {
            propagate(currNode, s)
          }
        }
      }
    )
  }

  override def computeReturnFlow(method: Method, curr: Statement, value: Val): Collection[_ <: State] = {
    flowFunctions.returnFlow(method, curr, value).stream()
      .map(x => new PopNode[Val](x, PDSSystem.CALLS))
      .collect(Collectors.toSet())
  }

  override def toString(): String = {
    "ForwardSolver: " + query
  }
}