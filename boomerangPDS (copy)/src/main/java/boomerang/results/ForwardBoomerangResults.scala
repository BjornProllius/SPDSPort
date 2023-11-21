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
import boomerang.util.DefaultValueMap
import boomerang.weights.DataFlowPathWeight
import boomerang.weights.PathConditionWeight.ConditionDomain
import com.google.common.base.Stopwatch
import com.google.common.collect.HashBasedTable
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import com.google.common.collect.Table
import java.util
import java.util.Map.Entry
import soot.jimple.IntConstant
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.State

class ForwardBoomerangResults[W <: Weight](private val query: Nothing, private var icfg: Nothing, private var cfg: Nothing, private val timedout: Boolean, queryToSolvers: Nothing, private val stats: Nothing, private var analysisWatch: Nothing, private var visitedMethods: Nothing, private val trackDataFlowPath: Boolean, private val pruneContradictoryDataFlowPath: Boolean, private var pruneImplictFlows: Boolean) extends Nothing(queryToSolvers) {
  stats.terminated(query, this)
  this.maxMemory = Util.getReallyUsedMemory
  private var maxMemory = 0L

  def getAnalysisWatch: Nothing = analysisWatch

  def isTimedout: Boolean = timedout

  def getObjectDestructingStatements: Nothing = {
    val solver = queryToSolvers.get(query)
    if (solver == null) return HashBasedTable.create
    val res = asStatementValWeightTable
    val visitedMethods = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (s <- res.rowKeySet) {
      visitedMethods.add(s.getMethod)
    }
    val forwardSolver = queryToSolvers.get(query)
    val destructingStatement = HashBasedTable.create
    import scala.collection.JavaConversions._
    for (flowReaches <- visitedMethods) {
      import scala.collection.JavaConversions._
      for (exitStmt <- icfg.getEndPointsOf(flowReaches)) {
        import scala.collection.JavaConversions._
        for (predOfExit <- exitStmt.getMethod.getControlFlowGraph.getPredsOf(exitStmt)) {
          val exitEdge = new Nothing(predOfExit, exitStmt)
          val escapes = Sets.newHashSet
          icfg.addCallerListener(new Nothing() {
            @Override def getObservedCallee: Nothing = flowReaches

            @Override def onCallerAdded(callSite: Nothing, m: Nothing): Unit = {
              val callee = callSite.getMethod
              if (visitedMethods.contains(callee)) {
                import scala.collection.JavaConversions._
                for (valAndW <- res.row(exitEdge).entrySet) {
                  escapes.addAll(forwardSolver.computeReturnFlow(flowReaches, exitStmt, valAndW.getKey))
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

  def asStatementValWeightTable: Nothing = asStatementValWeightTable(query)

  private def findLastUsage(exitStmt: Nothing, row: Nothing, destructingStatement: Nothing, forwardSolver: Nothing): Unit = {
    val worklist = Lists.newLinkedList
    worklist.add(exitStmt)
    val visited = Sets.newHashSet
    while (!worklist.isEmpty) {
      val curr = worklist.poll
      if (!visited.add(curr)) continue //todo: continue is not supported
      var valueUsedInStmt = false
      import scala.collection.JavaConversions._
      for (e <- row.entrySet) {
        if (curr.getTarget.uses(e.getKey)) {
          destructingStatement.put(curr, e.getKey, e.getValue)
          valueUsedInStmt = true
        }
      }
      if (!valueUsedInStmt && !curr.getTarget.isIdentityStmt

      /** Do not continue over CatchStmt */
      ) cfg.addPredsOfListener(new Nothing(curr.getStart) {
        @Override def getPredecessor(succ: Nothing): Unit = {
          worklist.add(new Nothing(succ, curr.getStart))
        }
      })
    }
  }

  def getStats: Nothing = stats

  def getInvokedMethodOnInstance: Nothing = {
    val invokedMethodsOnInstance = Maps.newHashMap
    if (query.cfgEdge.getStart.containsInvokeExpr) invokedMethodsOnInstance.put(query.cfgEdge, query.cfgEdge.getStart.getInvokeExpr.getMethod)
    queryToSolvers.get(query).getFieldAutomaton.registerListener((t, w, aut) => {
      def foo(t, w, aut) = {
        if (!t.getLabel.equals(Field.empty) || t.getStart.isInstanceOf[Nothing]) return
        val node = t.getStart.fact
        val fact = node.fact
        val currEdge = node.stmt
        val curr = currEdge.getStart
        if (curr.containsInvokeExpr) if (curr.getInvokeExpr.isInstanceInvokeExpr) {
          val base = curr.getInvokeExpr.getBase
          if (base.equals(fact)) invokedMethodsOnInstance.put(currEdge, curr.getInvokeExpr.getMethod)
        }
      }

      foo(t, w, aut)
    })
    invokedMethodsOnInstance
  }

  def getPotentialNullPointerDereferences: Nothing = {
    // FIXME this should be located nullpointer analysis
    val res = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (t <- queryToSolvers.get(query).getFieldAutomaton.getTransitions) {
      if (!t.getLabel.equals(Field.empty) || t.getStart.isInstanceOf[Nothing]) continue //todo: continue is not supported
      val nullPointerNode = t.getStart.fact
      if (NullPointerDereference.isNullPointerNode(nullPointerNode) && queryToSolvers.get(query).getReachedStates.contains(nullPointerNode)) res.add(nullPointerNode)
    }
    val resWithContext = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (r <- res) {
      // Context context = constructContextGraph(query, r);
      if (trackDataFlowPath) {
        val dataFlowPath = getDataFlowPathWeight(query, r)
        if (isValidPath(dataFlowPath)) {
          val p = transformPath(dataFlowPath.getAllStatements, r)
          resWithContext.add(new Nothing(query, r.stmt, r.fact, null, null, p))
        }
      }
      else {
        val dataFlowPath = Lists.newArrayList
        resWithContext.add(new Nothing(query, r.stmt, r.fact, null, null, dataFlowPath))
      }
    }
    val nullPointerResult = new Nothing(query, resWithContext, visitedMethods, timedout)
    nullPointerResult
  }

  private def isValidPath(dataFlowPath: Nothing): Boolean = {
    if (!pruneContradictoryDataFlowPath) return true
    val conditions = dataFlowPath.getConditions
    import scala.collection.JavaConversions._
    for (c <- conditions.entrySet) {
      if (contradiction(c.getKey, c.getValue, dataFlowPath.getEvaluationMap)) return false
    }
    true
  }

  private def getDataFlowPathWeight(query: Nothing, sinkLocation: Nothing): Nothing = {
    val callAut = queryToSolvers.getOrCreate(query).getCallAutomaton
    // Iterating over whole set to find the matching transition is not the most elegant solution....
    import scala.collection.JavaConversions._
    for (e <- callAut.getTransitionsToFinalWeights.entrySet) {
      val t = e.getKey
      if (t.getLabel.equals(new Nothing(Statement.epsilon, Statement.epsilon))) continue //todo: continue is not supported
      if (t.getStart.fact.isLocal && !t.getLabel.getMethod.equals(t.getStart.fact.m)) continue //todo: continue is not supported
      if (t.getStart.fact.equals(sinkLocation.fact) && t.getLabel.equals(sinkLocation.stmt)) if (e.getValue.isInstanceOf[Nothing]) {
        val v = e.getValue.asInstanceOf[Nothing]
        return v
      }
    }
    null
  }

  private def contradiction(ifStmt: Nothing, mustBeVal: Nothing, evaluationMap: Nothing): Boolean = {
    if (ifStmt.isIfStmt) {
      val ifStmt1 = ifStmt.getIfStmt
      import scala.collection.JavaConversions._
      for (t <- queryToSolvers.get(query).getFieldAutomaton.getTransitions) {
        if (!t.getStart.fact.stmt.equals(ifStmt)) continue //todo: continue is not supported
        if (!t.getLabel.equals(Field.empty) || t.getStart.isInstanceOf[Nothing]) continue //todo: continue is not supported
        val node = t.getStart.fact
        val fact = node.fact
        ifStmt1.evaluate(fact) match {
          case TRUE =>
            if (mustBeVal.equals(ConditionDomain.FALSE)) return true
          case FALSE =>
            if (mustBeVal.equals(ConditionDomain.TRUE)) return true
        }
      }
      if (pruneImplictFlows) {
        import scala.collection.JavaConversions._
        for (e <- evaluationMap.entrySet) {
          if (ifStmt1.uses(e.getKey)) {
            var eval: Nothing = null
            if (e.getValue.equals(ConditionDomain.TRUE)) {
              // Map first to JimpleVal
              eval = ifStmt1.evaluate(new Nothing(IntConstant.v(1), e.getKey.m))
            }
            else if (e.getValue.equals(ConditionDomain.FALSE)) {
              // Map first to JimpleVal
              eval = ifStmt1.evaluate(new Nothing(IntConstant.v(0), e.getKey.m))
            }
            if (eval != null) if (mustBeVal.equals(ConditionDomain.FALSE)) if (eval.equals(Evaluation.FALSE)) return true
            else if (mustBeVal.equals(ConditionDomain.TRUE)) if (eval.equals(Evaluation.TRUE)) return true
          }
        }
      }
    }
    false
  }

  private def transformPath(allStatements: Nothing, sinkLocation: Nothing) = {
    val res = Lists.newArrayList
    var index = 0
    import scala.collection.JavaConversions._
    for (x <- allStatements) {
      res.add(new Nothing(x.stmt, x.fact, {
        index += 1; index - 1
      }))
    }
    // TODO The analysis misses
    if (!allStatements.contains(sinkLocation)) res.add(new Nothing(sinkLocation.stmt, sinkLocation.fact, index))
    import scala.collection.JavaConversions._
    for (n <- res) {
      LOGGER.trace("Statement: {}, Variable {}, Index {}", n.getEdge, n.getVariable, n.stepIndex)
    }
    res
  }

  def getContext(node: Nothing): Nothing = constructContextGraph(query, node)

  def containsCallRecursion: Boolean = {
    import scala.collection.JavaConversions._
    for (e <- queryToSolvers.entrySet) {
      if (e.getValue.getCallAutomaton.containsLoop) return true
    }
    false
  }

  def containsFieldLoop: Boolean = {
    import scala.collection.JavaConversions._
    for (e <- queryToSolvers.entrySet) {
      if (e.getValue.getFieldAutomaton.containsLoop) return true
    }
    false
  }

  def getVisitedMethods: Nothing = visitedMethods

  def getMaxMemory: Long = maxMemory
}