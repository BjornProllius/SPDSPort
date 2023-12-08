package boomerang.flowfunction

import boomerang.{BoomerangOptions, scene}
import boomerang.scene.{ControlFlowGraph, Field, InvokeExpr, Method, Pair, Statement, StaticFieldVal, Val}
import boomerang.solver.{BackwardBoomerangSolver, Strategies}
import com.google.common.collect.{Multimap, Sets}
import java.util.{Collection, Collections, List, Set}
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.nodes.{ExclusionNode, Node, NodeWithLocation, PopNode, PushNode}
import wpds.impl.Weight
import wpds.interfaces.State

class DefaultBackwardFlowFunction extends IBackwardFlowFunction {

  private val options: BoomerangOptions = opts
  private var strategies: Strategies[Weight] = _
  private var solver: BackwardBoomerangSolver = _

  def returnFlow(callee: Method, returnStmt: Statement, returnedVal: Val): Set[Val] = {
    var out = Set[Val]()
    if (!callee.isStatic()) {
      if (callee.getThisLocal().equals(returnedVal)) {
        out += returnedVal
      }
    }
    for (param <- callee.getParameterLocals()) {
      if (param.equals(returnedVal)) {
        out += returnedVal
      }
    }
    if (callee.isStatic()) {
      out += returnedVal
    }
    out
  }

  def callFlow(callSite: Statement, fact: Val, callee: Method, calleeSp: Statement): Set[Val] = {
    if (!callSite.containsInvokeExpr()) {
      throw new RuntimeException("Call site does not contain an invoke expression.")
    }
    val invokeExpr = callSite.getInvokeExpr()
    var out = Set[Val]()
    if (invokeExpr.isInstanceInvokeExpr()) {
      if (invokeExpr.getBase().equals(fact) && !callee.isStatic()) {
        out += callee.getThisLocal()
      }
    }
    val parameterLocals = callee.getParameterLocals()
    var i = 0
    for (arg <- invokeExpr.getArgs()) {
      if (arg.equals(fact) && parameterLocals.size > i) {
        val param = parameterLocals.get(i)
        out += param
      }
      i += 1
    }

    if (callSite.isAssign() && calleeSp.isReturnStmt()) {
      if (callSite.getLeftOp().equals(fact)) {
        out += calleeSp.getReturnOp()
      }
    }
    if (fact.isStatic()) {
      out += fact.withNewMethod(callee)
    }
    out
  }

  override def normalFlow(currEdge: Edge, fact: Val): Collection[State] = {
    val curr = currEdge.getTarget()
    if (options.getAllocationVal(curr.getMethod(), curr, fact).isPresent()) {
      return Collections.emptySet()
    }
    if (curr.isThrowStmt()) {
      return Collections.emptySet()
    }
    var out = Set[State]()
    var leftSideMatches = false
    if (curr.isAssign()) {
      val leftOp = curr.getLeftOp()
      val rightOp = curr.getRightOp()
      if (leftOp.equals(fact)) {
        leftSideMatches = true
        if (curr.isFieldLoad()) {
          if (options.trackFields()) {
            val ifr = curr.getFieldLoad()
            if (!options.ignoreInnerClassFields() || !ifr.getY().isInnerClassField()) {
              out += new PushNode(currEdge, ifr.getX(), ifr.getY(), PDSSystem.FIELDS)
            }
          }
        } else if (curr.isStaticFieldLoad()) {
          if (options.trackFields()) {
            strategies.getStaticFieldStrategy().handleBackward(currEdge, curr.getLeftOp(), curr.getStaticField(), out)
          }
        } else if (rightOp.isArrayRef()) {
          val arrayBase = curr.getArrayBase()
          if (options.trackFields()) {
            strategies.getArrayHandlingStrategy().handleBackward(currEdge, arrayBase, out)
          }
        } else if (rightOp.isCast()) {
          out += new Node(currEdge, rightOp.getCastOp())
        } else if (curr.isPhiStatement()) {
          val phiVals = curr.getPhiVals()
          for (v <- phiVals) {
            out += new Node(currEdge, v)
          }
        } else {
          if (curr.isFieldLoadWithBase(fact)) {
            out += new ExclusionNode(currEdge, fact, curr.getLoadedField())
          } else {
            out += new Node(currEdge, rightOp)
          }
        }
      }
      if (curr.isFieldStore()) {
        val ifr = curr.getFieldStore()
        val base = ifr.getX()
        if (base.equals(fact)) {
          val succNode = new NodeWithLocation(currEdge, rightOp, ifr.getY())
          out += new PopNode(succNode, PDSSystem.FIELDS)
        }
      } else if (curr.isStaticFieldStore()) {
        val staticField = curr.getStaticField()
        if (fact.isStatic() && fact.equals(staticField)) {
          out += new Node(currEdge, rightOp)
        }
      } else if (leftOp.isArrayRef()) {
        val arrayBase = curr.getArrayBase()
        if (arrayBase.getX().equals(fact)) {
          val succNode = new NodeWithLocation(currEdge, rightOp, Field.array(arrayBase.getY()))
          out += new PopNode(succNode, PDSSystem.FIELDS)
        }
      }
    }
    if (!leftSideMatches) out += new Node(currEdge, fact)
    out
  }

  override def callToReturnFlow(edge: Edge, fact: Val): Collection[State] = {
    if (FlowFunctionUtils.isSystemArrayCopy(edge.getTarget().getInvokeExpr().getMethod())) {
      return systemArrayCopyFlow(edge, fact)
    }
    normalFlow(edge, fact)
  }

  override def setSolver(
    solver: BackwardBoomerangSolver,
    fieldLoadStatements: Multimap[Field, Statement],
    fieldStoreStatements: Multimap[Field, Statement]
  ): Unit = {
    this.solver = solver
    this.strategies = new Strategies(options, solver, fieldLoadStatements, fieldStoreStatements)
  }

  protected def systemArrayCopyFlow(edge: Edge, fact: Val): Collection[State] = {
    val callSite = edge.getTarget()
    if (fact.equals(callSite.getInvokeExpr().getArg(2))) {
      val arg = callSite.getInvokeExpr().getArg(0)
      return Collections.singleton(new Node(edge, arg))
    }
    Collections.emptySet()
  }
}