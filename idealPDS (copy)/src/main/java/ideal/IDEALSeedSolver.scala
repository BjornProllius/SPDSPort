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
package ideal

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.WeightedBoomerang
import boomerang.results.BackwardBoomerangResults
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import com.google.common.base.Stopwatch
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import java.util.Map.Entry
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.OneWeightFunctions
import sync.pds.solver.SyncPDSSolver.OnAddedSummaryListener
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import wpds.impl.NormalRule
import wpds.impl.PushRule
import wpds.impl.Rule
import wpds.impl.StackListener
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener
import wpds.interfaces.WPAUpdateListener

object IDEALSeedSolver {
  private val LOGGER = LoggerFactory.getLogger(classOf[IDEALSeedSolver[_ <: Nothing]])

  object Phases extends Enumeration {
    type Phases = Value
    val ObjectFlow, ValueFlow = Value
  }
}

class IDEALSeedSolver[W <: Weight](private val analysisDefinition: Nothing, private val seed: Nothing) {
  this.idealWeightFunctions = new Nothing(analysisDefinition.weightFunctions, analysisDefinition.enableStrongUpdates)
  this.one = analysisDefinition.weightFunctions.getOne
  this.phase1Solver = createSolver(IDEALSeedSolver.Phases.ObjectFlow)
  this.phase2Solver = createSolver(IDEALSeedSolver.Phases.ValueFlow)
  final private var idealWeightFunctions: Nothing = null
  final private var one: W = null
  final private var phase1Solver: Nothing = null
  final private var phase2Solver: Nothing = null
  final private val analysisStopwatch = Stopwatch.createUnstarted
  private val affectedStrongUpdateStmt = HashMultimap.create
  private val weakUpdates = Sets.newHashSet
  private var killedRules = 0

  final private class AddIndirectFlowAtCallSite private(private val callSite: Nothing, private val returnedFact: Nothing) extends Nothing {
    @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      if (t.getLabel.equals(callSite)) {
        idealWeightFunctions.addNonKillFlow(new Nothing(callSite, returnedFact))
        idealWeightFunctions.addIndirectFlow(new Nothing(callSite, returnedFact), new Nothing(callSite, t.getStart.fact))
      }
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (callSite == null) 0
      else callSite.hashCode)
      result = prime * result + (if (returnedFact == null) 0
      else returnedFact.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[IDEALSeedSolver[W]#AddIndirectFlowAtCallSite]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (callSite == null) if (other.callSite != null) return false
      else if (!callSite.equals(other.callSite)) return false
      if (returnedFact == null) if (other.returnedFact != null) return false
      else if (!returnedFact.equals(other.returnedFact)) return false
      true
    }

    private def getOuterType = thisIDEALSeedSolver
  }

  final private class TriggerBackwardQuery private(private val seedSolver: Nothing, private val boomerang: Nothing, private val strongUpdateNode: Nothing) extends Nothing(new Nothing(curr)) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (!t.getLabel.equals(Field.empty)) return
      addAffectedPotentialStrongUpdate(strongUpdateNode, strongUpdateNode.stmt)
      import scala.collection.JavaConversions._
      for (u <- strongUpdateNode.stmt.getMethod.getControlFlowGraph.getPredsOf(strongUpdateNode.stmt.getStart)) {
        val query = BackwardQuery.make(new Nothing(u, strongUpdateNode.stmt.getStart), strongUpdateNode.fact)
        val queryResults = boomerang.solve(query)
        val queryAllocationSites = queryResults.getAllocationSites.keySet
        setWeakUpdateIfNecessary()
        injectAliasesAtStrongUpdates(queryAllocationSites)
        injectAliasesAtStrongUpdatesAtCallStack(queryAllocationSites)
      }
    }

    private def injectAliasesAtStrongUpdatesAtCallStack(queryAllocationSites: Nothing): Unit = {
      seedSolver.getCallAutomaton.registerListener(new Nothing((seedSolver.getCallAutomaton, new Nothing(strongUpdateNode.fact), strongUpdateNode.stmt)) {
        @Override def anyContext(end: Nothing): Unit = {
        }

        @Override def stackElement(callSiteEdge: Nothing): Unit = {
          val callSite = callSiteEdge.getStart
          boomerang.checkTimeout
          addAffectedPotentialStrongUpdate(strongUpdateNode, callSiteEdge)
          import scala.collection.JavaConversions._
          for (e <- queryAllocationSites) {
            val solver = boomerang.getSolvers.get(e)
            solver.addApplySummaryListener((summaryCallSite, factInCallee, spInCallee, exitStmt, returnedFact) => {
              if (callSiteEdge.equals(summaryCallSite)) if (callSite.containsInvokeExpr) {
                if (returnedFact.isThisLocal) if (callSite.getInvokeExpr.isInstanceInvokeExpr) solver.getCallAutomaton.registerListener(new IDEALSeedSolver[W]#AddIndirectFlowAtCallSite(callSiteEdge, callSite.getInvokeExpr.getBase))
                if (returnedFact.isReturnLocal) if (callSite.isAssign) solver.getCallAutomaton.registerListener(new IDEALSeedSolver[W]#AddIndirectFlowAtCallSite(callSiteEdge, callSite.getLeftOp))
                var i = 0
                while (i < callSite.getInvokeExpr.getArgs.size) {
                  if (returnedFact.isParameterLocal(i)) solver.getCallAutomaton.registerListener(new IDEALSeedSolver[W]#AddIndirectFlowAtCallSite(callSiteEdge, callSite.getInvokeExpr.getArg(i)))
                  i += 1
                }
              }
            }.asInstanceOf[Nothing])
          }
        }
      })
    }

    private def injectAliasesAtStrongUpdates(queryAllocationSites: Nothing): Unit = {
      import scala.collection.JavaConversions._
      for (e <- queryAllocationSites) {
        val solver = boomerang.getSolvers.get(e)
        solver.getCallAutomaton.registerListener((t, w, aut) => {
          if (t.getLabel.equals(strongUpdateNode.stmt) /* && !t.getStart().fact().equals(curr.fact()) */ ) {
            idealWeightFunctions.addNonKillFlow(strongUpdateNode)
            idealWeightFunctions.addIndirectFlow(strongUpdateNode, new Nothing(strongUpdateNode.stmt, t.getStart.fact))
          }
        })
      }
    }

    private def setWeakUpdateIfNecessary(): Unit = {
      import scala.collection.JavaConversions._
      for (e <- boomerang.getSolvers.entrySet) {
        e.getValue.synchedEmptyStackReachable(strongUpdateNode, (targetFact) => {
          if (!e.getKey.asNode.equals(seed.asNode)) if (!e.getKey.asNode.fact.isNull) setWeakUpdate(strongUpdateNode)
        })
      }
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }
  }

  def run: Nothing = {
    IDEALSeedSolver.LOGGER.debug("Starting Phase 1 of IDEal")
    val resultPhase1 = runPhase(this.phase1Solver, IDEALSeedSolver.Phases.ObjectFlow)
    if (resultPhase1.isTimedout) {
      if (analysisStopwatch.isRunning) analysisStopwatch.stop
      throw new Nothing(this, this.phase1Solver, resultPhase1)
    }
    IDEALSeedSolver.LOGGER.debug("Starting Phase 2 of IDEal")
    val resultPhase2 = runPhase(this.phase2Solver, IDEALSeedSolver.Phases.ValueFlow)
    if (resultPhase2.isTimedout) {
      if (analysisStopwatch.isRunning) analysisStopwatch.stop
      throw new Nothing(this, this.phase2Solver, resultPhase2)
    }
    IDEALSeedSolver.LOGGER.debug("Killed Strong Update Rules {}", killedRules)
    resultPhase2
  }

  private def createSolver(phase: IDEALSeedSolver.Phases) = new Nothing((analysisDefinition.callGraph, analysisDefinition.getDataFlowScope, analysisDefinition.boomerangOptions)) {
    @Override protected def getForwardCallWeights(sourceQuery: Nothing): Nothing = {
      if (sourceQuery.equals(seed)) return idealWeightFunctions
      new Nothing(one)
    }

    @Override protected def getForwardFieldWeights: Nothing = return new Nothing(one)

    @Override protected def getBackwardFieldWeights: Nothing = return new Nothing(one)

    @Override protected def getBackwardCallWeights: Nothing = return new Nothing(one)

    @Override def preventCallRuleAdd(sourceQuery: Nothing, rule: Nothing): Boolean = {
      if (phase.equals(IDEALSeedSolver.Phases.ValueFlow) && sourceQuery.equals(seed)) if (preventStrongUpdateFlows(rule)) return true
      false
    }
  }

  protected def preventStrongUpdateFlows(rule: Nothing): Boolean = {
    if (rule.getS1.equals(rule.getS2)) if (idealWeightFunctions.isStrongUpdateStatement(rule.getL2)) if (idealWeightFunctions.isKillFlow(new Nothing(rule.getL2, rule.getS2.fact))) {
      killedRules += 1
      return true
    }
    if (rule.isInstanceOf[Nothing]) {
      val pushRule = rule.asInstanceOf[Nothing]
      val callSite = pushRule.getCallSite
      if (idealWeightFunctions.isStrongUpdateStatement(callSite)) if (idealWeightFunctions.isKillFlow(new Nothing(callSite, rule.getS1.fact))) {
        killedRules += 1
        return true
      }
    }
    false
  }

  private def runPhase(boomerang: Nothing, phase: IDEALSeedSolver.Phases) = {
    analysisStopwatch.start
    idealWeightFunctions.setPhase(phase)
    if (phase.equals(IDEALSeedSolver.Phases.ValueFlow)) registerIndirectFlowListener(boomerang.getSolvers.getOrCreate(seed))
    idealWeightFunctions.registerListener((curr) => {
      def foo(curr) = {
        if (phase.equals(IDEALSeedSolver.Phases.ValueFlow)) return
        val seedSolver = boomerang.getSolvers.getOrCreate(seed)
        seedSolver.getFieldAutomaton.registerListener(new IDEALSeedSolver[W]#TriggerBackwardQuery(seedSolver, boomerang, curr))
      }

      foo(curr)
    })
    val res = boomerang.solve(seed)
    analysisStopwatch.stop
    if (IDEALSeedSolver.LOGGER.isDebugEnabled) boomerang.printAllForwardCallAutomatonFlow
    boomerang.unregisterAllListeners
    res
  }

  protected def addAffectedPotentialStrongUpdate(strongUpdateNode: Nothing, stmt: Nothing): Unit = {
    if (affectedStrongUpdateStmt.put(strongUpdateNode, stmt)) {
      idealWeightFunctions.potentialStrongUpdate(stmt)
      if (weakUpdates.contains(strongUpdateNode)) idealWeightFunctions.weakUpdate(stmt)
    }
  }

  private def setWeakUpdate(curr: Nothing): Unit = {
    IDEALSeedSolver.LOGGER.debug("Weak update @ {}", curr)
    if (weakUpdates.add(curr)) {
      import scala.collection.JavaConversions._
      for (s <- Lists.newArrayList(affectedStrongUpdateStmt.get(curr))) {
        idealWeightFunctions.weakUpdate(s)
      }
    }
  }

  private def registerIndirectFlowListener(solver: Nothing): Unit = {
    val callAutomaton = solver.getCallAutomaton
    callAutomaton.registerListener((t, w, aut) => {
      def foo(t, w, aut) = {
        if (t.getStart.isInstanceOf[Nothing]) return
        val source = new Nothing(t.getLabel, t.getStart.fact)
        val indirectFlows = idealWeightFunctions.getAliasesFor(source)
        import scala.collection.JavaConversions._
        for (indirectFlow <- indirectFlows) {
          solver.addCallRule(new Nothing(new Nothing(source.fact), source.stmt, new Nothing(indirectFlow.fact), indirectFlow.stmt, one))
          solver.addFieldRule(new Nothing(solver.asFieldFact(source), solver.fieldWildCard, solver.asFieldFact(indirectFlow), solver.fieldWildCard, one))
        }
      }

      foo(t, w, aut)
    })
  }

  def getPhase1Solver: Nothing = phase1Solver

  def getPhase2Solver: Nothing = phase2Solver

  def getAnalysisStopwatch: Nothing = analysisStopwatch

  def getSeed: Nothing = seed
}