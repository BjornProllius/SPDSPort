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
package test

import boomerang.BoomerangOptions
import boomerang.DefaultBoomerangOptions
import boomerang.WeightedForwardQuery
import boomerang.debugger.Debugger
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.CallGraph
import boomerang.scene.CallGraph.Edge
import boomerang.scene.ControlFlowGraph
import boomerang.scene.DataFlowScope
import boomerang.scene.Method
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.JimpleMethod
import boomerang.scene.jimple.SootCallGraph
import com.google.common.collect.Lists
import ideal.IDEALAnalysis
import ideal.IDEALAnalysisDefinition
import ideal.IDEALResultHandler
import ideal.IDEALSeedSolver
import ideal.StoreIDEALResultHandler
import java.util
import java.util.Map.Entry
import soot.Scene
import soot.SceneTransformer
import sync.pds.solver.WeightFunctions
import test.ExpectedResults.InternalState
import test.core.selfrunning.AbstractTestingFramework
import test.core.selfrunning.ImprecisionException
import typestate.TransitionFunction
import typestate.finiteautomata.TypeStateMachineWeightFunctions

object IDEALTestingFramework {
  private val FAIL_ON_IMPRECISE = false

  /**
   * The methods parameter describes the variable that a query is issued for. Note: We misuse
   * the @Deprecated annotation to highlight the method in the Code.
   */
  protected def mayBeInErrorState(variable: Nothing): Unit = {
  }

  protected def mustBeInErrorState(variable: Nothing): Unit = {
  }

  protected def mayBeInAcceptingState(variable: Nothing): Unit = {
  }

  protected def mustBeInAcceptingState(variable: Nothing): Unit = {
  }

  protected def shouldNotBeAnalyzed(): Unit = {
  }
}

abstract class IDEALTestingFramework extends Nothing {
  protected var resultHandler = new Nothing
  protected var callGraph: Nothing = null
  protected var dataFlowScope: Nothing = null

  protected def getStateMachine: Nothing

  protected def createAnalysis = new Nothing(new Nothing() {
    @Override def generate(stmt: Nothing): Nothing = return thisIDEALTestingFramework.getStateMachine.generateSeed(stmt)

    @Override def weightFunctions: Nothing = return thisIDEALTestingFramework.getStateMachine

    @Override def debugger(solver: Nothing): Nothing = {
      return

      /**
       * VISUALIZATION ? new IDEVizDebugger<>(new File(
       * ideVizFile.getAbsolutePath().replace(".json", " " + solver.getSeed() + ".json")),
       * callGraph) :
       */
      new Nothing
    }

    @Override def getResultHandler: Nothing = return resultHandler

    @Override def boomerangOptions: Nothing = return new Nothing() {
      @Override def onTheFlyCallGraph: Boolean = return false

      def getStaticFieldStrategy: Nothing = return StaticFieldStrategy.FLOW_SENSITIVE

      @Override def allowMultipleQueries: Boolean = return true
    }

    @Override def callGraph: Nothing = return callGraph

    @Override protected def getDataFlowScope: Nothing = return dataFlowScope
  })

  @Override
  @throws[ImprecisionException]
  protected def createAnalysisTransformer: Nothing = new Nothing() {
    protected def internalTransform(phaseName: Nothing, @SuppressWarnings("rawtypes") options: Nothing): Unit = {
      BoomerangPretransformer.v.reset
      BoomerangPretransformer.v.apply
      callGraph = new Nothing
      dataFlowScope = SootDataFlowScope.make(Scene.v)
      analyze(JimpleMethod.of(sootTestMethod))
    }
  }

  protected def analyze(m: Nothing): Unit = {
    val expectedResults = parseExpectedQueryResults(m)
    val testingResultReporter = new Nothing(expectedResults)
    val seedToSolvers = executeAnalysis
    import scala.collection.JavaConversions._
    for (e <- seedToSolvers.entrySet) {
      testingResultReporter.onSeedFinished(e.getKey.asNode, e.getValue)
    }
    val unsound = Lists.newLinkedList
    val imprecise = Lists.newLinkedList
    import scala.collection.JavaConversions._
    for (r <- expectedResults) {
      if (r.isInstanceOf[Nothing]) throw new Nothing(r.toString)
    }
    import scala.collection.JavaConversions._
    for (r <- expectedResults) {
      if (!r.isSatisfied) unsound.add(r)
    }
    import scala.collection.JavaConversions._
    for (r <- expectedResults) {
      if (r.isImprecise) imprecise.add(r)
    }
    if (!unsound.isEmpty) throw new Nothing("Unsound results: " + unsound)
    if (!imprecise.isEmpty && IDEALTestingFramework.FAIL_ON_IMPRECISE) throw new Nothing("Imprecise results: " + imprecise)
  }

  protected def executeAnalysis: Nothing = {
    thisIDEALTestingFramework.createAnalysis.run
    resultHandler.getResults
  }

  private def parseExpectedQueryResults(sootTestMethod: Nothing) = {
    val results = new Nothing
    parseExpectedQueryResults(sootTestMethod, results, new Nothing)
    results
  }

  private def parseExpectedQueryResults(m: Nothing, queries: Nothing, visited: Nothing): Unit = {
    if (visited.contains(m)) return
    visited.add(m)
    import scala.collection.JavaConversions._
    for (stmt <- m.getStatements) {
      if (!stmt.containsInvokeExpr) continue //todo: continue is not supported
      import scala.collection.JavaConversions._
      for (callSite <- callGraph.edgesOutOf(stmt)) {
        parseExpectedQueryResults(callSite.tgt, queries, visited)
      }
      val invokeExpr = stmt.getInvokeExpr
      val invocationName = invokeExpr.getMethod.getName
      if (invocationName.equals("shouldNotBeAnalyzed")) queries.add(new Nothing(stmt))
      if (!invocationName.startsWith("mayBeIn") && !invocationName.startsWith("mustBeIn")) continue //todo: continue is not supported
      val `val` = invokeExpr.getArg(0)
      if (invocationName.startsWith("mayBeIn")) if (invocationName.contains("Error")) queries.add(new Nothing(stmt, `val`, InternalState.ERROR))
      else queries.add(new Nothing(stmt, `val`, InternalState.ACCEPTING))
      else if (invocationName.startsWith("mustBeIn")) if (invocationName.contains("Error")) queries.add(new Nothing(stmt, `val`, InternalState.ERROR))
      else queries.add(new Nothing(stmt, `val`, InternalState.ACCEPTING))
    }
  }

  /**
   * This method can be used in test cases to create branching. It is not optimized away.
   *
   * @return
   */
  protected def staticallyUnknown = true
}