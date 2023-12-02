/**
  * ******************************************************************************
  * Copyright (c) 2018
  * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
  * available under the terms of the Eclipse Public License 2.0 which is available at
  * http://www.eclipse.org/legal/epl-2.0.
  *
  * SPDX-License-Identifier: EPL-2.0
  *
  * Contributors: Johannes Spaeth - initial API and implementation
  * ******************************************************************************
  */
  package ideal

  import boomerang._
  import boomerang.debugger.Debugger
  import boomerang.results.ForwardBoomerangResults
  import boomerang.scene.ControlFlowGraph.Edge
  import boomerang.scene.{Field, Statement, Val}
  import boomerang.solver.AbstractBoomerangSolver
  import com.google.common.base.Stopwatch
  import com.google.common.collect.{HashMultimap, Lists, Multimap, Sets}
  import org.slf4j.{Logger, LoggerFactory}
  import sync.pds.solver.{OneWeightFunctions, WeightFunctions}
  import sync.pds.solver.nodes.{GeneratedState, INode, Node, SingleNode}
  import wpds.impl.{NormalRule, PushRule, Rule}
  import wpds.impl.Weight
  import wpds.impl.WeightedPAutomaton
  import wpds.interfaces.{WPAStateListener, WPAUpdateListener}
  
  import scala.collection.JavaConverters._
  
  class IDEALSeedSolver[W <: Weight](val analysisDefinition: IDEALAnalysisDefinition[W], val seed: ForwardQuery) {
  
    private val LOGGER: Logger = LoggerFactory.getLogger(classOf[IDEALSeedSolver[W]])
  
    private val idealWeightFunctions: IDEALWeightFunctions[W] =
      new IDEALWeightFunctions[W](analysisDefinition.weightFunctions(), analysisDefinition.enableStrongUpdates())
    private val one: W = analysisDefinition.weightFunctions().getOne()
    private val phase1Solver: WeightedBoomerang[W] = createSolver(Phases.ObjectFlow)
    private val phase2Solver: WeightedBoomerang[W] = createSolver(Phases.ValueFlow)
    private val analysisStopwatch: Stopwatch = Stopwatch.createUnstarted()
    private val affectedStrongUpdateStmt: Multimap[Node[Edge, Val], Edge] = HashMultimap.create()
    private val weakUpdates: Set[Node[Edge, Val]] = Sets.newHashSet()
    private var killedRules: Int = 0
  
    private class AddIndirectFlowAtCallSite(callSite: Edge, returnedFact: Val) extends WPAUpdateListener[Edge, INode[Val], W] {
  
      override def onWeightAdded(t: Transition[Edge, INode[Val]], w: W, aut: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
        if (t.getLabel == callSite) {
          idealWeightFunctions.addNonKillFlow(new Node(callSite, returnedFact))
          idealWeightFunctions.addIndirectFlow(new Node(callSite, returnedFact), new Node(callSite, t.getStart.fact()))
        }
      }
  
      override def hashCode(): Int = {
        val prime: Int = 31
        var result: Int = 1
        result = prime * result + getOuterType.hashCode
        result = prime * result + (if (callSite == null) 0 else callSite.hashCode)
        result = prime * result + (if (returnedFact == null) 0 else returnedFact.hashCode)
        result
      }
  
      override def equals(obj: Any): Boolean = {
        if (this == obj) return true
        if (obj == null) return false
        if (getClass != obj.getClass) return false
        val other: AddIndirectFlowAtCallSite = obj.asInstanceOf[AddIndirectFlowAtCallSite]
        if (getOuterType != other.getOuterType) return false
        if (callSite == null) {
          if (other.callSite != null) return false
        } else if (callSite != other.callSite) return false
        if (returnedFact == null) {
          if (other.returnedFact != null) return false
        } else if (returnedFact != other.returnedFact) return false
        true
      }
  
      private def getOuterType: IDEALSeedSolver[W] = IDEALSeedSolver.this
    }
  
    private class TriggerBackwardQuery(seedSolver: AbstractBoomerangSolver[W], boomerang: WeightedBoomerang[W], curr: Node[Edge, Val])
      extends WPAStateListener[Field, INode[Node[Edge, Val]], W](new SingleNode(curr)) {
  
      override def onOutTransitionAdded(t: Transition[Field, INode[Node[Edge, Val]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {
        if (!t.getLabel.equals(Field.empty())) {
          return
        }
        addAffectedPotentialStrongUpdate(curr, curr.stmt())
        for (u <- curr.stmt().getMethod.getControlFlowGraph.getPredsOf(curr.stmt().getStart).asScala) {
          val query: BackwardQuery = BackwardQuery.make(new Edge(u, curr.stmt().getStart), curr.fact())
          val queryResults: BackwardBoomerangResults[W] = boomerang.solve(query)
          val queryAllocationSites: Set[ForwardQuery] = queryResults.getAllocationSites.keySet()
          setWeakUpdateIfNecessary()
          injectAliasesAtStrongUpdates(queryAllocationSites)
          injectAliasesAtStrongUpdatesAtCallStack(queryAllocationSites)
        }
      }
  
      private def injectAliasesAtStrongUpdatesAtCallStack(queryAllocationSites: Set[ForwardQuery]): Unit = {
        seedSolver.getCallAutomaton.registerListener(
          new StackListener[Edge, INode[Val], W](seedSolver.getCallAutomaton, new SingleNode(curr.fact()), curr.stmt) {
            override def anyContext(end: Edge): Unit = {}
  
            override def stackElement(callSiteEdge: Edge): Unit = {
              val callSite: Statement = callSiteEdge.getStart
              boomerang.checkTimeout()
              addAffectedPotentialStrongUpdate(curr, callSiteEdge)
              for (e <- queryAllocationSites.asScala) {
                val solver: AbstractBoomerangSolver[W] = boomerang.getSolvers.get(e)
                solver.addApplySummaryListener(
                  (summaryCallSite: Edge, factInCallee: Val, spInCallee: Val, exitStmt: Statement, returnedFact: Val) => {
                    if (callSiteEdge.equals(summaryCallSite)) {
                      if (callSite.containsInvokeExpr) {
                        if (returnedFact.isThisLocal) {
                          if (callSite.getInvokeExpr.isInstanceInvokeExpr) {
                            solver.getCallAutomaton.registerListener(
                              new AddIndirectFlowAtCallSite(callSiteEdge, callSite.getInvokeExpr.getBase))
                          }
                        }
                        if (returnedFact.isReturnLocal) {
                          if (callSite.isAssign) {
                            solver.getCallAutomaton.registerListener(
                              new AddIndirectFlowAtCallSite(callSiteEdge, callSite.getLeftOp))
                          }
                        }
                        for (i <- 0 until callSite.getInvokeExpr.getArgs.size) {
                          if (returnedFact.isParameterLocal(i)) {
                            solver.getCallAutomaton.registerListener(
                              new AddIndirectFlowAtCallSite(callSiteEdge, callSite.getInvokeExpr.getArg(i)))
                          }
                        }
                      }
                    }
                  }
                )
              }
            }
          })
      }
  
      private def injectAliasesAtStrongUpdates(queryAllocationSites: Set[ForwardQuery]): Unit = {
        for (e <- queryAllocationSites.asScala) {
          val solver: AbstractBoomerangSolver[W] = boomerang.getSolvers.get(e)
          solver.getCallAutomaton.registerListener(
            (t: Transition[Edge, INode[Val]], w: W, aut: WeightedPAutomaton[Edge, INode[Val], W]) => {
              if (t.getLabel
                .equals(curr.stmt())) {
                idealWeightFunctions.addNonKillFlow(curr)
                idealWeightFunctions.addIndirectFlow(
                  curr, new Node(curr.stmt(), t.getStart.fact()))
              }
            })
        }
      }
  
      private def setWeakUpdateIfNecessary(): Unit = {
        for (e <- boomerang.getSolvers.entrySet().asScala) {
          val (key, value) = (e.getKey, e.getValue)
          value.synchedEmptyStackReachable(
            curr, (targetFact: Val) => {
              if (!key.asNode.equals(seed.asNode)) {
                if (!key.asNode.fact.isNull) {
                  setWeakUpdate(curr)
                }
              }
            })
        }
      }
  
      override def onInTransitionAdded(t: Transition[Field, INode[Node[Edge, Val]]], w: W, weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {}
    }
  
    object Phases extends Enumeration {
      type Phases = Value
      val ObjectFlow, ValueFlow = Value
    }
  
    def run(): ForwardBoomerangResults[W] = {
      LOGGER.debug("Starting Phase 1 of IDEal")
      val resultPhase1: ForwardBoomerangResults[W] = runPhase(phase1Solver, Phases.ObjectFlow)
      if (resultPhase1.isTimedout) {
        if (analysisStopwatch.isRunning) {
          analysisStopwatch.stop()
        }
        throw new IDEALSeedTimeout(this, this.phase1Solver, resultPhase1)
      }
      LOGGER.debug("Starting Phase 2 of IDEal")
      val resultPhase2: ForwardBoomerangResults[W] = runPhase(phase2Solver, Phases.ValueFlow)
      if (resultPhase2.isTimedout) {
        if (analysisStopwatch.isRunning) {
          analysisStopwatch.stop()
        }
        throw new IDEALSeedTimeout(this, this.phase2Solver, resultPhase2)
      }
      LOGGER.debug(s"Killed Strong Update Rules $killedRules")
      resultPhase2
    }
  
    private def createSolver(phase: Phases): WeightedBoomerang[W] = new WeightedBoomerang[W](
      analysisDefinition.callGraph(),
      analysisDefinition.getDataFlowScope(),
      analysisDefinition.boomerangOptions()
      ) {
    override def getForwardCallWeights(sourceQuery: ForwardQuery): WeightFunctions[Edge, Val, Edge, W] = {
      if (sourceQuery == seed) idealWeightFunctions
      else new OneWeightFunctions[W](one)
    }

    override def getForwardFieldWeights: WeightFunctions[Edge, Val, Field, W] =
      new OneWeightFunctions[W](one)

    override def getBackwardFieldWeights: WeightFunctions[Edge, Val, Field, W] =
      new OneWeightFunctions[W](one)

    override def getBackwardCallWeights: WeightFunctions[Edge, Val, Edge, W] =
      new OneWeightFunctions[W](one)

    override def preventCallRuleAdd(sourceQuery: ForwardQuery, rule: Rule[Edge, INode[Val], W]): Boolean = {
      if (phase == Phases.ValueFlow && sourceQuery == seed) {
        if (preventStrongUpdateFlows(rule)) {
          return true
        }
      }
      false
    }
  }

  private def preventStrongUpdateFlows(rule: Rule[Edge, INode[Val], W]): Boolean = {
    if (rule.getS1 == rule.getS2) {
      if (idealWeightFunctions.isStrongUpdateStatement(rule.getL2)) {
        if (idealWeightFunctions.isKillFlow(new Node(rule.getL2, rule.getS2.fact))) {
          killedRules += 1
          return true
        }
      }
    }
    if (rule.isInstanceOf[PushRule[Edge, INode[Val], W]]) {
      val pushRule: PushRule[Edge, INode[Val], W] = rule.asInstanceOf[PushRule[Edge, INode[Val], W]]
      val callSite: Edge = pushRule.getCallSite
      if (idealWeightFunctions.isStrongUpdateStatement(callSite)) {
        if (idealWeightFunctions.isKillFlow(new Node(callSite, rule.getS1.fact))) {
          killedRules += 1
          return true
        }
      }
    }
    false
  }

  private def runPhase(boomerang: WeightedBoomerang[W], phase: Phases): ForwardBoomerangResults[W] = {
    analysisStopwatch.start()
    idealWeightFunctions.setPhase(phase)

    if (phase == Phases.ValueFlow) {
      registerIndirectFlowListener(boomerang.getSolvers.getOrCreate(seed))
    }

    idealWeightFunctions.registerListener(
      curr => {
        if (phase == Phases.ValueFlow) {
          return
        }
        val seedSolver: AbstractBoomerangSolver[W] = boomerang.getSolvers.getOrCreate(seed)
        seedSolver.getFieldAutomaton.registerListener(new TriggerBackwardQuery(seedSolver, boomerang, curr))
      })

    val res: ForwardBoomerangResults[W] = boomerang.solve(seed)
    analysisStopwatch.stop()
    if (LOGGER.isDebugEnabled) {
      boomerang.printAllForwardCallAutomatonFlow()
    }
    boomerang.unregisterAllListeners()
    res
  }

  private def addAffectedPotentialStrongUpdate(strongUpdateNode: Node[Edge, Val], stmt: Edge): Unit = {
    if (affectedStrongUpdateStmt.put(strongUpdateNode, stmt)) {
      idealWeightFunctions.potentialStrongUpdate(stmt)
      if (weakUpdates.contains(strongUpdateNode)) {
        idealWeightFunctions.weakUpdate(stmt)
      }
    }
  }

  private def setWeakUpdate(curr: Node[Edge, Val]): Unit = {
    LOGGER.debug(s"Weak update @ $curr")
    if (weakUpdates.add(curr)) {
      for (s <- Lists.newArrayList(affectedStrongUpdateStmt.get(curr))) {
        idealWeightFunctions.weakUpdate(s)
      }
    }
  }

  private def registerIndirectFlowListener(solver: AbstractBoomerangSolver[W]): Unit = {
    val callAutomaton: WeightedPAutomaton[Edge, INode[Val], W] = solver.getCallAutomaton
    callAutomaton.registerListener(
      (t: Transition[Edge, INode[Val]], w: W, aut: WeightedPAutomaton[Edge, INode[Val], W]) => {
        if (t.getStart.isInstanceOf[GeneratedState]) return
        val source: Node[Edge, Val] = new Node(t.getLabel, t.getStart.fact())
        val indirectFlows: Iterable[Node[Edge, Val]] = idealWeightFunctions.getAliasesFor(source)
        for (indirectFlow <- indirectFlows) {
          solver.addCallRule(
            new NormalRule[Edge, INode[Val], W](
              new SingleNode(source.fact),
              source.stmt,
              new SingleNode(indirectFlow.fact),
              indirectFlow.stmt,
              one)
          )
          solver.addFieldRule(
            new NormalRule[Edge, INode[Val], W](
              solver.asFieldFact(source),
              solver.fieldWildCard,
              solver.asFieldFact(indirectFlow),
              solver.fieldWildCard,
              one)
          )
        }
      }
    )
  }

  def getPhase1Solver: WeightedBoomerang[W] = phase1Solver

  def getPhase2Solver: WeightedBoomerang[W] = phase2Solver

  def getAnalysisStopwatch: Stopwatch = analysisStopwatch

  def getSeed: ForwardQuery = seed
}
