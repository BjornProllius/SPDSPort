package boomerang.flowfunction

import boomerang.BoomerangOptions
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import boomerang.solver.BackwardBoomerangSolver
import boomerang.solver.Strategies
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import java.util.Collections
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.nodes.ExclusionNode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.NodeWithLocation
import sync.pds.solver.nodes.PopNode
import sync.pds.solver.nodes.PushNode
import wpds.impl.Weight
import wpds.interfaces.State

class DefaultBackwardFlowFunction(private val options: Nothing) extends Nothing {
  private var strategies: Nothing = null
  private var solver: Nothing = null

  @Override def returnFlow(callee: Nothing, returnStmt: Nothing, returnedVal: Nothing): Nothing = {
    val out = Sets.newHashSet
    if (!callee.isStatic) if (callee.getThisLocal.equals(returnedVal)) out.add(returnedVal)
    import scala.collection.JavaConversions._
    for (param <- callee.getParameterLocals) {
      if (param.equals(returnedVal)) out.add(returnedVal)
    }
    if (callee.isStatic) out.add(returnedVal)
    out
  }

  @Override def callFlow(callSite: Nothing, fact: Nothing, callee: Nothing, calleeSp: Nothing): Nothing = {
    if (!callSite.containsInvokeExpr) throw new Nothing("Call site does not contain an invoke expression.")
    val invokeExpr = callSite.getInvokeExpr
    val out = Sets.newHashSet
    if (invokeExpr.isInstanceInvokeExpr) if (invokeExpr.getBase.equals(fact) && !callee.isStatic) out.add(callee.getThisLocal)
    val parameterLocals = callee.getParameterLocals
    var i = 0
    import scala.collection.JavaConversions._
    for (arg <- invokeExpr.getArgs) {
      if (arg.equals(fact) && parameterLocals.size > i) {
        val param = parameterLocals.get(i)
        out.add(param)
      }
      i += 1
    }
    if (callSite.isAssign && calleeSp.isReturnStmt) if (callSite.getLeftOp.equals(fact)) out.add(calleeSp.getReturnOp)
    if (fact.isStatic) out.add(fact.withNewMethod(callee))
    out
  }

  @Override def normalFlow(currEdge: Nothing, fact: Nothing): Nothing = {
    val curr = currEdge.getTarget
    if (options.getAllocationVal(curr.getMethod, curr, fact).isPresent) return Collections.emptySet
    if (curr.isThrowStmt) return Collections.emptySet
    val out = Sets.newHashSet
    var leftSideMatches = false
    if (curr.isAssign) {
      val leftOp = curr.getLeftOp
      val rightOp = curr.getRightOp
      if (leftOp.equals(fact)) {
        leftSideMatches = true
        if (curr.isFieldLoad) if (options.trackFields) {
          val ifr = curr.getFieldLoad
          if (!options.ignoreInnerClassFields || !ifr.getY.isInnerClassField) out.add(new Nothing(currEdge, ifr.getX, ifr.getY, PDSSystem.FIELDS))
        }
        else if (curr.isStaticFieldLoad) if (options.trackFields) strategies.getStaticFieldStrategy.handleBackward(currEdge, curr.getLeftOp, curr.getStaticField, out)
        else if (rightOp.isArrayRef) {
          val arrayBase = curr.getArrayBase
          if (options.trackFields) strategies.getArrayHandlingStrategy.handleBackward(currEdge, arrayBase, out)
        }
        else if (rightOp.isCast) out.add(new Nothing(currEdge, rightOp.getCastOp))
        else if (curr.isPhiStatement) {
          val phiVals = curr.getPhiVals
          import scala.collection.JavaConversions._
          for (v <- phiVals) {
            out.add(new Nothing(currEdge, v))
          }
        }
        else if (curr.isFieldLoadWithBase(fact)) out.add(new Nothing(currEdge, fact, curr.getLoadedField))
        else out.add(new Nothing(currEdge, rightOp))
      }
      if (curr.isFieldStore) {
        val ifr = curr.getFieldStore
        val base = ifr.getX
        if (base.equals(fact)) {
          val succNode = new Nothing(currEdge, rightOp, ifr.getY)
          out.add(new Nothing(succNode, PDSSystem.FIELDS))
        }
      }
      else if (curr.isStaticFieldStore) {
        val staticField = curr.getStaticField
        if (fact.isStatic && fact.equals(staticField)) out.add(new Nothing(currEdge, rightOp))
      }
      else if (leftOp.isArrayRef) {
        val arrayBase = curr.getArrayBase
        if (arrayBase.getX.equals(fact)) {
          val succNode = new Nothing(currEdge, rightOp, Field.array(arrayBase.getY))
          out.add(new Nothing(succNode, PDSSystem.FIELDS))
        }
      }
    }
    if (!leftSideMatches) out.add(new Nothing(currEdge, fact))
    out
  }

  @Override def callToReturnFlow(edge: Nothing, fact: Nothing): Nothing = {
    if (FlowFunctionUtils.isSystemArrayCopy(edge.getTarget.getInvokeExpr.getMethod)) return systemArrayCopyFlow(edge, fact)
    normalFlow(edge, fact)
  }

  @Override def setSolver(solver: Nothing, fieldLoadStatements: Nothing, fieldStoreStatements: Nothing): Unit = {
    this.solver = solver
    this.strategies = new Nothing(options, solver, fieldLoadStatements, fieldStoreStatements)
  }

  protected def systemArrayCopyFlow(edge: Nothing, fact: Nothing): Nothing = {
    val callSite = edge.getTarget
    if (fact.equals(callSite.getInvokeExpr.getArg(2))) {
      val arg = callSite.getInvokeExpr.getArg(0)
      return Collections.singleton(new Nothing(edge, arg))
    }
    Collections.emptySet
  }
}