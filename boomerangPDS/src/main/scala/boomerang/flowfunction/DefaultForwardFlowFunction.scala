import boomerang.{BoomerangOptions, ForwardQuery}
import boomerang.scene.{ControlFlowGraph, Field, InvokeExpr, Method, Pair, Statement, StaticFieldVal, Val}
import boomerang.solver.{ForwardBoomerangSolver, Strategies}
import com.google.common.collect.{Multimap, Sets}
import java.util.{Collection, Collections, List, Set}
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.nodes.{ExclusionNode, Node, NodeWithLocation, PopNode, PushNode}
import wpds.interfaces.State

class DefaultForwardFlowFunction extends IForwardFlowFunction {

  private val options: BoomerangOptions = opts
  private var strategies: Strategies = _
  private var solver: ForwardBoomerangSolver = _

  def returnFlow(method: Method, curr: Statement, value: Val): Set[Val] = {
    var out = Set[Val]()
    if (curr.isThrowStmt() && !options.throwFlows()) {
      return Set.empty[Val]
    }
    if (curr.isReturnStmt()) {
      if (curr.getReturnOp().equals(value)) {
        out += value
      }
    }
    if (!method.isStatic()) {
      if (method.getThisLocal().equals(value)) {
        out += value
      }
    }
    for (param <- method.getParameterLocals()) {
      if (param.equals(value)) {
        out += value
      }
    }
    if (value.isStatic()) {
      out += value
    }
    out
  }

  override def callFlow(callSite: Statement, fact: Val, callee: Method): Set[Val] = {
    if (!callSite.containsInvokeExpr()) {
      throw new RuntimeException("Call site does not contain an invoke expression.")
    }
    if (callee.isStaticInitializer()) {
      return Set.empty[Val]
    }
    var out = Set[Val]()
    val invokeExpr = callSite.getInvokeExpr()
    if (invokeExpr.isInstanceInvokeExpr()) {
      if (invokeExpr.getBase().equals(fact) && !callee.isStatic()) {
        out += callee.getThisLocal()
      }
    }
    var i = 0
    val parameterLocals = callee.getParameterLocals()
    for (arg <- invokeExpr.getArgs()) {
      if (arg.equals(fact) && parameterLocals.size > i) {
        out += parameterLocals.get(i)
      }
      i += 1
    }
    if (fact.isStatic()) {
      out += fact.withNewMethod(callee)
    }
    out
  }

  override def normalFlow(query: ForwardQuery, nextEdge: Edge, fact: Val): Set[State] = {
    val succ = nextEdge.getStart()
    var out = Set[State]()
    if (killFlow(succ, fact)) {
      return out
    }
    if (!succ.isFieldWriteWithBase(fact)) {
      // always maintain data-flow if not a field write // killFlow has
      // been taken care of
      if (!options.trackReturnOfInstanceOf()
          || !(query.getType().isNullType() && succ.isInstanceOfStatement(fact))) {
        out += new Node(nextEdge, fact)
      }
    } else {
      out += new ExclusionNode(nextEdge, fact, succ.getWrittenField())
    }
    if (succ.isAssign()) {
      val leftOp = succ.getLeftOp()
      val rightOp = succ.getRightOp()
      if (rightOp.equals(fact)) {
        if (succ.isFieldStore()) {
          val ifr = succ.getFieldStore()
          if (options.trackFields()) {
            if (!options.ignoreInnerClassFields() || !ifr._2.isInnerClassField()) {
              out += new PushNode(nextEdge, ifr._1, ifr._2, PDSSystem.FIELDS)
            }
          }
        } else if (succ.isStaticFieldStore()) {
          val sf = succ.getStaticField()
          if (options.trackFields()) {
            strategies.getStaticFieldStrategy().handleForward(nextEdge, rightOp, sf, out)
          }
        } else if (leftOp.isArrayRef()) {
          val arrayBase = succ.getArrayBase()
          if (options.trackFields()) {
            strategies.getArrayHandlingStrategy().handleForward(nextEdge, arrayBase, out)
          }
        } else {
          out += new Node(nextEdge, leftOp)
        }
      }
      if (succ.isFieldLoad()) {
        val ifr = succ.getFieldLoad()
        if (ifr._1.equals(fact)) {
          val succNode = new NodeWithLocation(nextEdge, leftOp, ifr._2)
          out += new PopNode(succNode, PDSSystem.FIELDS)
        }
      } else if (succ.isStaticFieldLoad()) {
        val sf = succ.getStaticField()
        if (fact.isStatic() && fact.equals(sf)) {
          out += new Node(nextEdge, leftOp)
        }
      } else if (rightOp.isArrayRef()) {
        val arrayBase = succ.getArrayBase()
        if (arrayBase._1.equals(fact)) {
          val succNode = new NodeWithLocation(nextEdge, leftOp, Field.array(arrayBase._2))
          out += new PopNode(succNode, PDSSystem.FIELDS)
        }
      } else if (rightOp.isCast()) {
        if (rightOp.getCastOp().equals(fact)) {
          out += new Node(nextEdge, leftOp)
        }
      } else if (rightOp.isInstanceOfExpr()
          && query.getType().isNullType()
          && options.trackReturnOfInstanceOf()) {
        if (rightOp.getInstanceOfOp().equals(fact)) {
          out += new Node(nextEdge, fact.withSecondVal(leftOp))
        }
      } else if (succ.isPhiStatement()) {
        val phiVals = succ.getPhiVals()
        if (phiVals.contains(fact)) {
          out += new Node(nextEdge, succ.getLeftOp())
        }
      }
    }
    out
  }

  protected def killFlow(curr: Statement, value: Val): Boolean = {
    if (curr.isThrowStmt() || curr.isCatchStmt()) {
      return true
    }

    if (curr.isAssign()) {
      // Kill x at any statement x = * during propagation.
      if (curr.getLeftOp().equals(value)) {
        // But not for a statement x = x.f
        if (curr.isFieldLoad()) {
          val ifr = curr.getFieldLoad()
          if (ifr.getX().equals(value)) {
            return false
          }
        }
        return true
      }
      if (curr.isStaticFieldStore()) {
        val sf = curr.getStaticField()
        if (value.isStatic() && value.equals(sf)) {
          return true
        }
      }
    }
    false
  }

  override def callToReturnFlow(query: ForwardQuery, edge: Edge, fact: Val): Collection[State] = {
    if (FlowFunctionUtils.isSystemArrayCopy(edge.getStart().getInvokeExpr().getMethod())) {
      return systemArrayCopyFlow(edge, fact)
    }
    normalFlow(query, edge, fact)
  }

  protected def systemArrayCopyFlow(edge: Edge, value: Val): Collection[State] = {
    val callSite = edge.getStart()
    if (value.equals(callSite.getInvokeExpr().getArg(0))) {
      val arg = callSite.getInvokeExpr().getArg(2)
      return Collections.singleton(new Node(edge, arg))
    }
    Collections.emptySet()
  }

  override def setSolver(
    solver: ForwardBoomerangSolver,
    fieldLoadStatements: Multimap[Field, Statement],
    fieldStoreStatements: Multimap[Field, Statement]
  ): Unit = {
    this.solver = solver
    this.strategies = new Strategies(options, solver, fieldLoadStatements, fieldStoreStatements)
  }
}
