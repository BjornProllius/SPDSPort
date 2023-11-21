package boomerang.guided

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.guided.Specification.Parameter
import boomerang.guided.Specification.QueryDirection
import boomerang.guided.Specification.QuerySelector
import boomerang.guided.Specification.SootMethodWithSelector
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.JimpleStatement
import com.google.common.collect.Sets
import java.util
import java.util.Optional
import java.util.stream.Collectors
import soot.jimple.Stmt

class SimpleSpecificationGuidedManager(private val spec: Nothing) extends Nothing {
  @Override def onForwardFlow(query: Nothing, dataFlowEdge: Nothing, dataFlowVal: Nothing): Nothing = {
    val stmt = dataFlowEdge.getStart
    val res = Sets.newHashSet
    if (stmt.containsInvokeExpr) {
      val selectors = spec.getMethodAndQueries.stream.filter((x) => isInOnList(x, stmt, dataFlowVal, QueryDirection.FORWARD)).collect(Collectors.toSet)
      import scala.collection.JavaConversions._
      for (sel <- selectors) {
        res.addAll(createNewQueries(sel, stmt))
      }
    }
    res
  }

  @Override def onBackwardFlow(query: Nothing, dataFlowEdge: Nothing, dataFlowVal: Nothing): Nothing = {
    val stmt = dataFlowEdge.getStart
    val res = Sets.newHashSet
    if (stmt.containsInvokeExpr) {
      val selectors = spec.getMethodAndQueries.stream.filter((x) => isInOnList(x, stmt, dataFlowVal, QueryDirection.BACKWARD)).collect(Collectors.toSet)
      import scala.collection.JavaConversions._
      for (sel <- selectors) {
        res.addAll(createNewQueries(sel, stmt))
      }
    }
    res
  }

  private def createNewQueries(sel: Nothing, stmt: Nothing) = {
    val results = Sets.newHashSet
    val method = stmt.getMethod
    import scala.collection.JavaConversions._
    for (qSel <- sel.getGo) {
      val parameterVal = getParameterVal(stmt, qSel.argumentSelection)
      if (parameterVal.isPresent) if (qSel.direction eq QueryDirection.BACKWARD) {
        import scala.collection.JavaConversions._
        for (pred <- method.getControlFlowGraph.getPredsOf(stmt)) {
          results.add(BackwardQuery.make(new Nothing(pred, stmt), parameterVal.get))
        }
      }
      else if (qSel.direction eq QueryDirection.FORWARD) {
        import scala.collection.JavaConversions._
        for (succ <- method.getControlFlowGraph.getSuccsOf(stmt)) {
          results.add(new Nothing(new Nothing(stmt, succ), new Nothing(parameterVal.get, stmt, parameterVal.get)))
        }
      }
    }
    results
  }

  def isInOnList(methodSelector: Nothing, stmt: Nothing, fact: Nothing, direction: Nothing): Boolean = {
    if (stmt.isInstanceOf[Nothing]) {
      // This only works for Soot propagations
      val jimpleStmt = stmt.asInstanceOf[Nothing].getDelegate
      if (jimpleStmt.getInvokeExpr.getMethod.getSignature.equals(methodSelector.getSootMethod)) {
        val on = methodSelector.getOn
        return isInList(on, direction, stmt, fact)
      }
    }
    false
  }

  private def isInList(list: Nothing, direction: Nothing, stmt: Nothing, fact: Nothing) = list.stream.anyMatch((sel) => (sel.direction eq direction) && isParameter(stmt, fact, sel.argumentSelection))

  private def isParameter(stmt: Nothing, fact: Nothing, argumentSelection: Nothing): Boolean = {
    if (stmt.getInvokeExpr.isInstanceInvokeExpr && argumentSelection.equals(Parameter.base)) return stmt.getInvokeExpr.getBase.equals(fact)
    if (argumentSelection.equals(Parameter.returnParam)) return stmt.isAssign && stmt.getLeftOp.equals(fact)
    stmt.getInvokeExpr.getArgs.size > argumentSelection.getValue && argumentSelection.getValue >= 0 && stmt.getInvokeExpr.getArg(argumentSelection.getValue).equals(fact)
  }

  private def getParameterVal(stmt: Nothing, selector: Nothing): Nothing = {
    if (stmt.containsInvokeExpr && !stmt.getInvokeExpr.isStaticInvokeExpr && selector.equals(Parameter.base)) return Optional.of(stmt.getInvokeExpr.getBase)
    if (stmt.isAssign && selector.equals(Parameter.returnParam)) return Optional.of(stmt.getLeftOp)
    if (stmt.getInvokeExpr.getArgs.size > selector.getValue && selector.getValue >= 0) return Optional.of(stmt.getInvokeExpr.getArg(selector.getValue))
    Optional.empty
  }
}