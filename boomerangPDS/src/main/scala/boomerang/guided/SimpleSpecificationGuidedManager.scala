package boomerang.guided

import boomerang.{BackwardQuery, ForwardQuery, Query}
import boomerang.guided.Specification.{Parameter, QueryDirection, QuerySelector, SootMethodWithSelector}
import boomerang.scene.{AllocVal, ControlFlowGraph, Method, Statement, Val}
import boomerang.scene.jimple.JimpleStatement
import com.google.common.collect.Sets

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Try

class SimpleSpecificationGuidedManager extends IDemandDrivenGuidedManager{

  class SimpleSpecificationGuidedManager(spec: Specification) extends IDemandDrivenGuidedManager {

    override def onForwardFlow(query: ForwardQuery, dataFlowEdge: ControlFlowGraph.Edge, dataFlowVal: Val): Collection[Query] = {
      val stmt = dataFlowEdge.getStart
      val res = Sets.newHashSet[Query]()
      if (stmt.containsInvokeExpr()) {
        val selectors = spec.getMethodAndQueries.asScala.filter(x => isInOnList(x, stmt, dataFlowVal, QueryDirection.FORWARD)).toSet
        for (sel <- selectors) {
          res.addAll(createNewQueries(sel, stmt))
        }
      }
      res
    }
  
    override def onBackwardFlow(query: BackwardQuery, dataFlowEdge: ControlFlowGraph.Edge, dataFlowVal: Val): Collection[Query] = {
      val stmt = dataFlowEdge.getStart
      val res = Sets.newHashSet[Query]()
      if (stmt.containsInvokeExpr()) {
        val selectors = spec.getMethodAndQueries.asScala.filter(x => isInOnList(x, stmt, dataFlowVal, QueryDirection.BACKWARD)).toSet
        for (sel <- selectors) {
          res.addAll(createNewQueries(sel, stmt))
        }
      }
      res
    }
  
    private def createNewQueries(sel: SootMethodWithSelector, stmt: Statement): Collection[Query] = {
      val results = Sets.newHashSet[Query]()
      val method = stmt.getMethod
      for (qSel <- sel.getGo.asScala) {
        val parameterVal = getParameterVal(stmt, qSel.argumentSelection)
        if (parameterVal.isPresent) {
          if (qSel.direction == QueryDirection.BACKWARD) {
            for (pred <- method.getControlFlowGraph.getPredsOf(stmt).asScala) {
              results.add(BackwardQuery.make(new Edge(pred, stmt), parameterVal.get))
            }
          } else if (qSel.direction == QueryDirection.FORWARD) {
            for (succ <- method.getControlFlowGraph.getSuccsOf(stmt).asScala) {
              results.add(
                new ForwardQuery(
                  new Edge(stmt, succ),
                  new AllocVal(parameterVal.get, stmt, parameterVal.get)))
            }
          }
        }
      }
      results
    }

  def isInOnList(methodSelector: SootMethodWithSelector, stmt: Statement, fact: Val, direction: QueryDirection): Boolean = {
    stmt match {
      case jimpleStmt: JimpleStatement =>
        if (jimpleStmt.getDelegate.getInvokeExpr.getMethod.getSignature.equals(methodSelector.getSootMethod)) {
          val on = methodSelector.getOn
          isInList(on, direction, stmt, fact)
        } else false
      case _ => false
    }
  }

  private def isInList(list: Collection[QuerySelector], direction: QueryDirection, stmt: Statement, fact: Val): Boolean = {
    list.asScala.exists(sel => sel.direction == direction && isParameter(stmt, fact, sel.argumentSelection))
  }

  private def isParameter(stmt: Statement, fact: Val, argumentSelection: Parameter): Boolean = {
    if (stmt.getInvokeExpr.isInstanceInvokeExpr && argumentSelection.equals(Parameter.base())) {
      stmt.getInvokeExpr.getBase.equals(fact)
    } else if (argumentSelection.equals(Parameter.returnParam())) {
      stmt.isAssign && stmt.getLeftOp.equals(fact)
    } else {
      stmt.getInvokeExpr.getArgs.size > argumentSelection.getValue &&
        argumentSelection.getValue >= 0 &&
        stmt.getInvokeExpr.getArg(argumentSelection.getValue).equals(fact)
    }
  }

  private def getParameterVal(stmt: Statement, selector: Parameter): Option[Val] = {
    if (stmt.containsInvokeExpr() && !stmt.getInvokeExpr.isStaticInvokeExpr && selector.equals(Parameter.base())) {
      Some(stmt.getInvokeExpr.getBase)
    } else if (stmt.isAssign && selector.equals(Parameter.returnParam())) {
      Some(stmt.getLeftOp)
    } else if (stmt.getInvokeExpr.getArgs.size > selector.getValue && selector.getValue >= 0) {
      Some(stmt.getInvokeExpr.getArg(selector.getValue))
    } else {
      None
    }
  }}
