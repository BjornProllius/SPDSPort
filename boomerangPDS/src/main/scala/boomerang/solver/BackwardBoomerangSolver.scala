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

import boomerang.{BackwardQuery, BoomerangOptions}
import boomerang.callgraph.{CalleeListener, ObservableICFG}
import boomerang.controlflowgraph.{ObservableControlFlowGraph, PredecessorListener, SuccessorListener}
import boomerang.flowfunction.IBackwardFlowFunction
import boomerang.scene._
import boomerang.scene.ControlFlowGraph.Edge
import com.google.common.collect.{Multimap, Sets}
import java.util.AbstractMap.SimpleEntry
import java.util.{Collection, Map, Set}
import java.util.stream.Collectors
import org.slf4j.{Logger, LoggerFactory}
import sync.pds.solver.nodes._
import wpds.impl.{NestedWeightedPAutomatons, Transition}
import wpds.impl.Weight
import wpds.interfaces.{Location, State}

abstract class BackwardBoomerangSolver[W <: Weight] (
  icfg: ObservableICFG[Statement, Method],
  cfg: ObservableControlFlowGraph,
  genField: Map[(INode[Node[ControlFlowGraph.Edge, Val]], Field), INode[Node[ControlFlowGraph.Edge, Val]]],
  query: BackwardQuery,
  options: BoomerangOptions,
  callSummaries: NestedWeightedPAutomatons[ControlFlowGraph.Edge, INode[Val], W],
  fieldSummaries: NestedWeightedPAutomatons[Field, INode[Node[ControlFlowGraph.Edge, Val]], W],
  scope: DataFlowScope,
  backwardFlowFunction: IBackwardFlowFunction,
  fieldLoadStatements: Multimap[Field, Statement],
  fieldStoreStatements: Multimap[Field, Statement],
  propagationType: Type
  ) extends AbstractBoomerangSolver[W](icfg, cfg, genField, options, callSummaries, fieldSummaries, scope, propagationType) {

  private val LOGGER = LoggerFactory.getLogger(classOf[BackwardBoomerangSolver[_]])
  private val query = query
  private val flowFunction = backwardFlowFunction

  flowFunction.setSolver(this, fieldLoadStatements, fieldStoreStatements)

  private def notUsedInMethod(m: Method, curr: Statement, value: Val): Boolean = {
    if (value.isStatic()) return false
    if (!m.getLocals().contains(value)) return true
    false
  }

  def generateFieldState(d: INode[Node[ControlFlowGraph.Edge, Val]], loc: Field): INode[Node[ControlFlowGraph.Edge, Val]] = {
    val e = (d, loc)
    if (!generatedFieldState.contains(e)) {
      generatedFieldState.put(
        e, new GeneratedState[Node[ControlFlowGraph.Edge, Val]](new SingleNode[Node[ControlFlowGraph.Edge, Val]](new Node[ControlFlowGraph.Edge, Val](epsilonStmt(), Val.zero())), loc))
    }
    generatedFieldState(e)
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

  override protected def computeReturnFlow(method: Method, callerReturnStatement: Statement, value: Val): Collection[_ <: State] = {
    flowFunction.returnFlow(method, callerReturnStatement, value).stream()
      .map(x => new PopNode(x, PDSSystem.CALLS))
      .collect(Collectors.toSet())
  }

  protected def callFlow(caller: Method, curr: Node[Edge, Val], callSite: Statement): Unit = {
    icfg.addCalleeListener(new CallSiteCalleeListener(curr, caller))
    val invokeExpr = callSite.getInvokeExpr()
    if (dataFlowScope.isExcluded(invokeExpr.getMethod())) {
      byPassFlowAtCallsite(caller, curr)
    }
  }

  private def byPassFlowAtCallsite(caller: Method, curr: Node[Edge, Val]): Unit = {
    for (returnSite <- curr.stmt().getStart().getMethod().getControlFlowGraph().getPredsOf(curr.stmt().getStart())) {
      val res = flowFunction.callToReturnFlow(new Edge(returnSite, curr.stmt().getStart()), curr.fact())
        .stream()
        .collect(Collectors.toSet())
      for (s <- res) {
        propagate(curr, s)
      }
    }
  }

  override def computeSuccessor(node: Node[Edge, Val]): Unit = {
    LOGGER.trace("BW: Computing successor of {} for {}", node, this)
    val edge = node.stmt()
    val value = node.fact()
    assert(!value.isInstanceOf[AllocVal])
    val method = edge.getStart().getMethod()
    if (method == null) return
    if (dataFlowScope.isExcluded(method)) return
    if (notUsedInMethod(method, edge.getStart(), value)) {
      return
    }
    if (edge.getStart().containsInvokeExpr() && edge.getStart().uses(value) && INTERPROCEDURAL) {
      callFlow(method, node, edge.getStart())
    } else if (icfg.isExitStmt(edge.getStart())) {
      returnFlow(method, node)
    } else {
      normalFlow(method, node)
    }
  }

  protected def normalFlow(method: Method, currNode: Node[ControlFlowGraph.Edge, Val]): Unit = {
    val curr = currNode.stmt()
    val value = currNode.fact()
    for (pred <- curr.getStart().getMethod().getControlFlowGraph().getPredsOf(curr.getStart())) {
      val flow = computeNormalFlow(method, new Edge(pred, curr.getStart()), value)
      for (s <- flow) {
        propagate(currNode, s)
      }
    }
  }

  protected def computeCallFlow(callSiteEdge: Edge, fact: Val, callee: Method, calleeStartEdge: Edge): Collection[_ <: State] = {
    val calleeSp = calleeStartEdge.getTarget()
    flowFunction.callFlow(callSiteEdge.getTarget(), fact, callee, calleeSp).stream()
      .map(x => new PushNode(calleeStartEdge, x, callSiteEdge, PDSSystem.CALLS))
      .collect(Collectors.toSet())
  }

  override def processPush(curr: Node[Edge, Val], location: Location, succ: PushNode[Edge, Val, _], system: PDSSystem): Unit = {
    if (PDSSystem.CALLS == system) {
      if (!succ.location().getTarget().equals(curr.stmt().getStart())
          || !curr.stmt().getStart().containsInvokeExpr()) {
        throw new RuntimeException("Invalid push rule")
      }
    }
    super.processPush(curr, location, succ, system)
  }

  override protected def computeNormalFlow(method: Method, currEdge: Edge, fact: Val): Collection[State] = {
    flowFunction.normalFlow(currEdge, fact).stream().collect(Collectors.toSet())
  }

  override def applyCallSummary(callSiteEdge: Edge, factAtSpInCallee: Val, spInCallee: Edge, exitStmt: Edge, exitingFact: Val): Unit = {
    val out = Sets.newHashSet[Node[Edge, Val]]()
    val callSite = callSiteEdge.getTarget()
    if (callSite.containsInvokeExpr()) {
      if (exitingFact.isThisLocal()) {
        if (callSite.getInvokeExpr().isInstanceInvokeExpr()) {
          out.add(new Node(callSiteEdge, callSite.getInvokeExpr().getBase()))
        }
      }
      if (exitingFact.isReturnLocal()) {
        if (callSite.isAssign()) {
          out.add(new Node(callSiteEdge, callSite.getLeftOp()))
        }
      }
      for (i <- 0 until callSite.getInvokeExpr().getArgs().size()) {
        if (exitingFact.isParameterLocal(i)) {
          out.add(new Node(callSiteEdge, callSite.getInvokeExpr().getArg(i)))
        }
      }
    }
    for (xs <- out) {
      addNormalCallFlow(new Node(callSiteEdge, exitingFact), xs)
      addNormalFieldFlow(new Node(exitStmt, exitingFact), xs)
    }
  }

  override protected def propagateUnbalancedToCallSite(callSite: Statement, transInCallee: Transition[Edge, INode[Val]]): Unit = {
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
                val curr = new Node[ControlFlowGraph.Edge, Val](new Edge(callSite, succ), query.var())

                val callTrans = new Transition[ControlFlowGraph.Edge, INode[Val]](
                  wrap(curr.fact()),
                  curr.stmt(),
                  generateCallState(wrap(curr.fact()), curr.stmt()))
                callAutomaton.addTransition(callTrans)
                callAutomaton.addUnbalancedState(
                  generateCallState(wrap(curr.fact()), curr.stmt()), target)

                val s = new PushNode[Edge, Val, Edge](
                  target.location(),
                  target.node().fact(),
                  new Edge(pred, callSite),
                  PDSSystem.CALLS)
                propagate(curr, s)
              }
            })
        }
      })
  }

  private final class CallSiteCalleeListener(curr: Node[Edge, Val], caller: Method) extends CalleeListener[Statement, Method] {
    private val callSite = curr.stmt().getStart()

    override def getObservedCaller(): Statement = {
      callSite
    }

    override def onCalleeAdded(callSite: Statement, callee: Method): Unit = {
      if (callee.isStaticInitializer()) {
        return
      }
      for (calleeSp <- icfg.getStartPointsOf(callee)) {
        for (predOfCall <- callSite.getMethod().getControlFlowGraph().getPredsOf(callSite)) {
          val res = computeCallFlow(
            new Edge(predOfCall, callSite),
            curr.fact(),
            callee,
            new Edge(calleeSp, calleeSp))
          for (o <- res) {
            BackwardBoomerangSolver.this.propagate(curr, o)
          }
        }
      }
    }

    override def onNoCalleeFound(): Unit = {
      byPassFlowAtCallsite(caller, curr)
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType().hashCode()
      result = prime * result + (if (caller == null) 0 else caller.hashCode())
      result = prime * result + (if (curr == null) 0 else curr.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      if (this == obj) return true
      if (obj == null) return false
      if (getClass() != obj.getClass()) return false
      val other = obj.asInstanceOf[CallSiteCalleeListener]
      if (!getOuterType().equals(other.getOuterType())) return false
      if (caller == null) {
        if (other.caller != null) return false
      } else if (!caller.equals(other.caller)) return false
      if (curr == null) {
        if (other.curr != null) return false
      } else if (!curr.equals(other.curr)) return false
      true
    }

    private def getOuterType(): BackwardBoomerangSolver = {
      BackwardBoomerangSolver.this
    }
  }

  override def toString(): String = {
    "BackwardBoomerangSolver{" + "query=" + query + '}'
  }
}