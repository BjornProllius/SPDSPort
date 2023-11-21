package boomerang.flowfunction

import boomerang.BoomerangOptions
import boomerang.ForwardQuery
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.StaticFieldVal
import boomerang.scene.Val
import boomerang.solver.ForwardBoomerangSolver
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
import wpds.interfaces.State

class DefaultForwardFlowFunction(private val options: Nothing) extends Nothing {
  private var strategies: Nothing = null
  private var solver: Nothing = null

  @Override def returnFlow(method: Nothing, curr: Nothing, value: Nothing): Nothing = {
    val out = Sets.newHashSet
    if (curr.isThrowStmt && !options.throwFlows) return Collections.emptySet
    if (curr.isReturnStmt) if (curr.getReturnOp.equals(value)) out.add(value)
    if (!method.isStatic) if (method.getThisLocal.equals(value)) out.add(value)
    import scala.collection.JavaConversions._
    for (param <- method.getParameterLocals) {
      if (param.equals(value)) out.add(value)
    }
    if (value.isStatic) out.add(value)
    out
  }

  @Override def callFlow(callSite: Nothing, fact: Nothing, callee: Nothing): Nothing = {
    if (!callSite.containsInvokeExpr) throw new Nothing("Call site does not contain an invoke expression.")
    if (callee.isStaticInitializer) return Collections.emptySet
    val out = Sets.newHashSet
    val invokeExpr = callSite.getInvokeExpr
    if (invokeExpr.isInstanceInvokeExpr) if (invokeExpr.getBase.equals(fact) && !callee.isStatic) out.add(callee.getThisLocal)
    var i = 0
    val parameterLocals = callee.getParameterLocals
    import scala.collection.JavaConversions._
    for (arg <- invokeExpr.getArgs) {
      if (arg.equals(fact) && parameterLocals.size > i) out.add(parameterLocals.get(i))
      i += 1
    }
    if (fact.isStatic) out.add(fact.withNewMethod(callee))
    out
  }

  @Override def normalFlow(query: Nothing, nextEdge: Nothing, fact: Nothing): Nothing = {
    val succ = nextEdge.getStart
    val out = Sets.newHashSet
    if (killFlow(succ, fact)) return out
    if (!succ.isFieldWriteWithBase(fact)) {
      // always maintain data-flow if not a field write // killFlow has
      // been taken care of
      if (!options.trackReturnOfInstanceOf || !(query.getType.isNullType && succ.isInstanceOfStatement(fact))) out.add(new Nothing(nextEdge, fact))
    }
    else out.add(new Nothing(nextEdge, fact, succ.getWrittenField))
    if (succ.isAssign) {
      val leftOp = succ.getLeftOp
      val rightOp = succ.getRightOp
      if (rightOp.equals(fact)) if (succ.isFieldStore) {
        val ifr = succ.getFieldStore
        if (options.trackFields) if (!options.ignoreInnerClassFields || !ifr.getY.isInnerClassField) out.add(new Nothing(nextEdge, ifr.getX, ifr.getY, PDSSystem.FIELDS))
      }
      else if (succ.isStaticFieldStore) {
        val sf = succ.getStaticField
        if (options.trackFields) strategies.getStaticFieldStrategy.handleForward(nextEdge, rightOp, sf, out)
      }
      else if (leftOp.isArrayRef) {
        val arrayBase = succ.getArrayBase
        if (options.trackFields) strategies.getArrayHandlingStrategy.handleForward(nextEdge, arrayBase, out)
      }
      else out.add(new Nothing(nextEdge, leftOp))
      if (succ.isFieldLoad) {
        val ifr = succ.getFieldLoad
        if (ifr.getX.equals(fact)) {
          val succNode = new Nothing(nextEdge, leftOp, ifr.getY)
          out.add(new Nothing(succNode, PDSSystem.FIELDS))
        }
      }
      else if (succ.isStaticFieldLoad) {
        val sf = succ.getStaticField
        if (fact.isStatic && fact.equals(sf)) out.add(new Nothing(nextEdge, leftOp))
      }
      else if (rightOp.isArrayRef) {
        val arrayBase = succ.getArrayBase
        if (arrayBase.getX.equals(fact)) {
          val succNode = new Nothing(nextEdge, leftOp, Field.array(arrayBase.getY))
          out.add(new Nothing(succNode, PDSSystem.FIELDS))
        }
      }
      else if (rightOp.isCast) if (rightOp.getCastOp.equals(fact)) out.add(new Nothing(nextEdge, leftOp))
      else if (rightOp.isInstanceOfExpr && query.getType.isNullType && options.trackReturnOfInstanceOf) if (rightOp.getInstanceOfOp.equals(fact)) out.add(new Nothing(nextEdge, fact.withSecondVal(leftOp)))
      else if (succ.isPhiStatement) {
        val phiVals = succ.getPhiVals
        if (phiVals.contains(fact)) out.add(new Nothing(nextEdge, succ.getLeftOp))
      }
    }
    out
  }

  protected def killFlow(curr: Nothing, value: Nothing): Boolean = {
    if (curr.isThrowStmt || curr.isCatchStmt) return true
    if (curr.isAssign) {
      // Kill x at any statement x = * during propagation.
      if (curr.getLeftOp.equals(value)) {
        // But not for a statement x = x.f
        if (curr.isFieldLoad) {
          val ifr = curr.getFieldLoad
          if (ifr.getX.equals(value)) return false
        }
        return true
      }
      if (curr.isStaticFieldStore) {
        val sf = curr.getStaticField
        if (value.isStatic && value.equals(sf)) return true
      }
    }
    false
  }

  @Override def callToReturnFlow(query: Nothing, edge: Nothing, fact: Nothing): Nothing = {
    if (FlowFunctionUtils.isSystemArrayCopy(edge.getStart.getInvokeExpr.getMethod)) return systemArrayCopyFlow(edge, fact)
    normalFlow(query, edge, fact)
  }

  protected def systemArrayCopyFlow(edge: Nothing, value: Nothing): Nothing = {
    val callSite = edge.getStart
    if (value.equals(callSite.getInvokeExpr.getArg(0))) {
      val arg = callSite.getInvokeExpr.getArg(2)
      return Collections.singleton(new Nothing(edge, arg))
    }
    Collections.emptySet
  }

  @Override def setSolver(solver: Nothing, fieldLoadStatements: Nothing, fieldStoreStatements: Nothing): Unit = {
    this.solver = solver
    this.strategies = new Nothing(options, solver, fieldLoadStatements, fieldStoreStatements)
  }
}