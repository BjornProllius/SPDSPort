package boomerang.results

import boomerang.ForwardQuery
import boomerang.Util
import boomerang.callgraph.CallerListener
import boomerang.callgraph.ObservableICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.controlflowgraph.PredecessorListener
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DeclaredMethod
import boomerang.scene.Field
import boomerang.scene.IfStatement
import boomerang.scene.IfStatement.Evaluation
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.JimpleVal
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import boomerang.stats.IBoomerangStats
import boomerang.weights.DataFlowPathWeight
import boomerang.weights.PathConditionWeight.ConditionDomain
import com.google.common.base.Stopwatch
import com.google.common.collect.HashBasedTable
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import com.google.common.collect.Table
import java.util.LinkedList
import java.util.List
import java.util.Map
import java.util.Map.Entry
import java.util.Set
import soot.jimple.IntConstant
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.State

class ForwardBoomerangResults[W <: Weight](query: ForwardQuery,
                                           icfg: ObservableICFG[Statement, Method],
                                           cfg: ObservableControlFlowGraph,
                                           timedout: Boolean,
                                           queryToSolvers: DefaultValueMap[ForwardQuery, ForwardBoomerangSolver[W]],
                                           stats: IBoomerangStats[W],
                                           analysisWatch: Stopwatch,
                                           visitedMethods: Set[Method],
                                           trackDataFlowPath: Boolean,
                                           pruneContradictoryDataFlowPath: Boolean,
                                           pruneImplictFlows: Boolean)
  extends AbstractBoomerangResults[W](queryToSolvers) {

  stats.terminated(query, this)
  private val maxMemory: Long = Util.getReallyUsedMemory()

  def getAnalysisWatch: Stopwatch = analysisWatch

  def isTimedout: Boolean = timedout

  def getObjectDestructingStatements: Table[Edge, Val, W] = {
    val solver = queryToSolvers.get(query)
    if (solver == null) {
      return HashBasedTable.create()
    }
    val res = asStatementValWeightTable()
    val visitedMethods = Sets.newHashSet()
    for (s <- res.rowKeySet()) {
      visitedMethods.add(s.getMethod())
    }
    val forwardSolver = queryToSolvers.get(query)
    val destructingStatement = HashBasedTable.create[Edge, Val, W]()
    for (flowReaches <- visitedMethods) {
      for (exitStmt <- icfg.getEndPointsOf(flowReaches)) {
        for (predOfExit <- exitStmt.getMethod().getControlFlowGraph().getPredsOf(exitStmt)) {
          val exitEdge = new Edge(predOfExit, exitStmt)
          val escapes = Sets.newHashSet[State]()
          icfg.addCallerListener(
            new CallerListener[Statement, Method]() {
              override def getObservedCallee: Method = flowReaches

              override def onCallerAdded(callSite: Statement, m: Method): Unit = {
                val callee = callSite.getMethod()
                if (visitedMethods.contains(callee)) {
                  for ((valAndW, _) <- res.row(exitEdge)) {
                    escapes.addAll(forwardSolver.computeReturnFlow(flowReaches, exitStmt, valAndW))
                  }
                }
              }
            })

          if (escapes.isEmpty) {
            val row = res.row(exitEdge)
            findLastUsage(exitEdge, row, destructingStatement, forwardSolver)
          }
        }
      }
    }

    destructingStatement
  }

  def asStatementValWeightTable(): Table[Edge, Val, W] = {
    asStatementValWeightTable(query)
  }

  private def findLastUsage(
      exitStmt: Edge,
      row: Map[Val, W],
      destructingStatement: Table[Edge, Val, W],
      forwardSolver: ForwardBoomerangSolver[W]): Unit = {
    val worklist = new LinkedList[Edge]()
    worklist.add(exitStmt)
    val visited = new HashSet[Edge]()
    while (!worklist.isEmpty) {
      val curr = worklist.poll()
      if (!visited.add(curr)) {
        continue
      }
      var valueUsedInStmt = false
      for ((k, v) <- row) {
        if (curr.getTarget.uses(k)) {
          destructingStatement.put(curr, k, v)
          valueUsedInStmt = true
        }
      }
      if (!valueUsedInStmt && !curr.getTarget.isIdentityStmt()) {
        cfg.addPredsOfListener(
          new PredecessorListener(curr.getStart) {
            override def getPredecessor(succ: Statement): Unit = {
              worklist.add(new Edge(succ, curr.getStart))
            }
          }
        )
      }
    }
  }

  def getStats(): IBoomerangStats[W] = {
    stats
  }

  def getInvokedMethodOnInstance(): Map[Edge, DeclaredMethod] = {
    val invokedMethodsOnInstance = new HashMap[Edge, DeclaredMethod]()
    if (query.cfgEdge.getStart.containsInvokeExpr()) {
      invokedMethodsOnInstance.put(
        query.cfgEdge, query.cfgEdge.getStart.getInvokeExpr.getMethod)
    }
    queryToSolvers
      .get(query)
      .getFieldAutomaton()
      .registerListener(
        (t, w, aut) => {
          if (!t.getLabel.equals(Field.empty()) || t.getStart.isInstanceOf[GeneratedState]) {
            return
          }
          val node = t.getStart.fact()
          val fact = node.fact()
          val currEdge = node.stmt()
          val curr = currEdge.getStart
          if (curr.containsInvokeExpr()) {
            if (curr.getInvokeExpr.isInstanceInvokeExpr()) {
              val base = curr.getInvokeExpr.getBase()
              if (base.equals(fact)) {
                invokedMethodsOnInstance.put(currEdge, curr.getInvokeExpr.getMethod())
              }
            }
          }
        }
      )
    invokedMethodsOnInstance
  }

  def getPotentialNullPointerDereferences(): QueryResults = {
    // FIXME this should be located nullpointer analysis
    val res = new HashSet[Node[Edge, Val]]
    for (t <- queryToSolvers.get(query).getFieldAutomaton().getTransitions) {
      if (!t.getLabel.equals(Field.empty()) || t.getStart.isInstanceOf[GeneratedState]) {
        continue
      }
      val nullPointerNode = t.getStart.fact()
      if (NullPointerDereference.isNullPointerNode(nullPointerNode)
          && queryToSolvers.get(query).getReachedStates.contains(nullPointerNode)) {
        res.add(nullPointerNode)
      }
    }
    val resWithContext = new HashSet[AffectedLocation]
    for (r <- res) {
      // Context context = constructContextGraph(query, r);
      if (trackDataFlowPath) {
        val dataFlowPath = getDataFlowPathWeight(query, r)
        if (isValidPath(dataFlowPath)) {
          val p = transformPath(dataFlowPath.getAllStatements, r)
          resWithContext.add(new NullPointerDereference(query, r.stmt(), r.fact(), null, null, p))
        }
      } else {
        val dataFlowPath = new ArrayList[PathElement]
        resWithContext.add(
          new NullPointerDereference(query, r.stmt(), r.fact(), null, null, dataFlowPath))
      }
    }
    val nullPointerResult =
      new QueryResults(query, resWithContext, visitedMethods, timedout)
    nullPointerResult
  }

  private def isValidPath(dataFlowPath: DataFlowPathWeight): Boolean = {
    if (!pruneContradictoryDataFlowPath) {
      return true
    }
    val conditions = dataFlowPath.getConditions
    for ((c, v) <- conditions) {
      if (contradiction(c, v, dataFlowPath.getEvaluationMap)) {
        return false
      }
    }
    true
  }

  private def getDataFlowPathWeight(
      query: ForwardQuery, sinkLocation: Node[Edge, Val]): DataFlowPathWeight = {
    val callAut =
      queryToSolvers.getOrCreate(query).getCallAutomaton()
    // Iterating over whole set to find the matching transition is not the most elegant solution....
    for ((t, w) <- callAut.getTransitionsToFinalWeights) {
      if (t.getLabel.equals(new Edge(Statement.epsilon(), Statement.epsilon()))) {
        continue
      }
      if (t.getStart.fact().isLocal()
          && !t.getLabel.getMethod.equals(t.getStart.fact().m())) {
        continue
      }
      if (t.getStart.fact().equals(sinkLocation.fact())
          && t.getLabel.equals(sinkLocation.stmt())) {
        if (w.isInstanceOf[DataFlowPathWeight]) {
          val v = w.asInstanceOf[DataFlowPathWeight]
          return v
        }
      }
    }
    null
  }

  private def contradiction(
      ifStmt: Statement, mustBeVal: ConditionDomain, evaluationMap: Map[Val, ConditionDomain]): Boolean = {
    if (ifStmt.isIfStmt()) {
      val ifStmt1 = ifStmt.getIfStmt()
      for (t <- queryToSolvers.get(query).getFieldAutomaton().getTransitions()) {
        if (!t.getStart().fact().stmt().equals(ifStmt)) {
          continue
        }
        if (!t.getLabel().equals(Field.empty()) || t.getStart().isInstanceOf[GeneratedState]) {
          continue
        }
        val node = t.getStart().fact()
        val fact = node.fact()
        ifStmt1.evaluate(fact) match {
          case Evaluation.TRUE =>
            if (mustBeVal.equals(ConditionDomain.FALSE)) {
              return true
            }
          case Evaluation.FALSE =>
            if (mustBeVal.equals(ConditionDomain.TRUE)) {
              return true
            }
        }
      }
      if (pruneImplictFlows) {
        for ((k, v) <- evaluationMap) {
          if (ifStmt1.uses(k)) {
            var eval: Evaluation = null
            if (v.equals(ConditionDomain.TRUE)) {
              eval = ifStmt1.evaluate(new JimpleVal(IntConstant.v(1), k.m()))
            } else if (v.equals(ConditionDomain.FALSE)) {
              eval = ifStmt1.evaluate(new JimpleVal(IntConstant.v(0), k.m()))
            }
            if (eval != null) {
              if (mustBeVal.equals(ConditionDomain.FALSE)) {
                if (eval.equals(Evaluation.FALSE)) {
                  return true
                }
              } else if (mustBeVal.equals(ConditionDomain.TRUE)) {
                if (eval.equals(Evaluation.TRUE)) {
                  return true
                }
              }
            }
          }
        }
      }
    }
    false
  }

  private def transformPath(
      allStatements: List[Node[Edge, Val]], sinkLocation: Node[Edge, Val]): List[PathElement] = {
    val res = new ArrayList[PathElement]()
    var index = 0
    for (x <- allStatements) {
      res.add(new PathElement(x.stmt(), x.fact(), index))
      index += 1
    }
    if (!allStatements.contains(sinkLocation)) {
      res.add(new PathElement(sinkLocation.stmt(), sinkLocation.fact(), index))
    }
    for (n <- res) {
      LOGGER.trace("Statement: {}, Variable {}, Index {}", n.getEdge(), n.getVariable(), n.stepIndex())
    }
    res
  }

  def getContext(node: Node[Edge, Val]): Context = {
    constructContextGraph(query, node)
  }

  def containsCallRecursion(): Boolean = {
    for ((k, v) <- queryToSolvers) {
      if (v.getCallAutomaton().containsLoop()) {
        return true
      }
    }
    false
  }

  def containsFieldLoop(): Boolean = {
    for ((k, v) <- queryToSolvers) {
      if (v.getFieldAutomaton().containsLoop()) {
        return true
      }
    }
    false
  }

  def getVisitedMethods(): Set[Method] = {
    visitedMethods
  }

  def getMaxMemory(): Long = {
    maxMemory
  }
}