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
package test.core

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.DefaultBoomerangOptions
import boomerang.Query
import boomerang.WeightedBoomerang
import boomerang.results.BackwardBoomerangResults
import boomerang.scene.AnalysisScope
import boomerang.scene.CallGraph
import boomerang.scene.DataFlowScope
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.SootCallGraph
import com.google.common.base.Joiner
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import java.util.Map.Entry
import java.util.concurrent.TimeUnit
import org.junit.Rule
import org.junit.rules.Timeout
import soot.RefType
import soot.Scene
import soot.SceneTransformer
import soot.SootClass
import soot.jimple.NewExpr
import test.core.selfrunning.AbstractTestingFramework
import wpds.impl.Weight

object MultiQueryBoomerangTest {
  private val FAIL_ON_IMPRECISE = false

  /**
   * The methods parameter describes the variable that a query is issued for. Note: We misuse
   * the @Deprecated annotation to highlight the method in the Code.
   */
  def queryFor1(variable: Nothing, interfaceType: Nothing): Unit = {
  }

  def queryFor2(variable: Nothing, interfaceType: Nothing): Unit = {
  }

  def accessPathQueryFor(variable: Nothing, aliases: Nothing): Unit = {
  }
}

class MultiQueryBoomerangTest extends Nothing {
  @Rule var timeout = new Nothing(10000000, TimeUnit.MILLISECONDS)
  private val allocationSites: Nothing = null
  protected var queryForCallSites: Nothing = null
  protected var expectedAllocsForQuery: Nothing = HashMultimap.create
  protected var unsoundErrors: Nothing = Sets.newHashSet
  protected var imprecisionErrors: Nothing = Sets.newHashSet
  private var callGraph: Nothing = null
  private var dataFlowScope: Nothing = null
  protected var analysisTimeout: Int = 300 * 1000
  private var solver: Nothing = null

  protected def createAnalysisTransformer: Nothing = new Nothing() {
    protected def internalTransform(phaseName: Nothing, @SuppressWarnings("rawtypes") options: Nothing): Unit = {
      BoomerangPretransformer.v.reset
      BoomerangPretransformer.v.apply
      callGraph = new Nothing
      dataFlowScope = SootDataFlowScope.make(Scene.v)
      val analysisScope = new Nothing(callGraph, new Nothing("queryFor.*"))
      queryForCallSites = analysisScope.computeSeeds
      import scala.collection.JavaConversions._
      for (q <- queryForCallSites) {
        val arg2 = q.cfgEdge.getStart.getInvokeExpr.getArg(1)
        if (arg2.isClassConstant) {
          val analysis = new Nothing(callGraph, new Nothing(arg2.getClassConstantType.toString))
          expectedAllocsForQuery.putAll(q, analysis.computeSeeds)
        }
      }
      runDemandDrivenBackward()
      if (!unsoundErrors.isEmpty) throw new Nothing(Joiner.on("\n").join(unsoundErrors))
      if (!imprecisionErrors.isEmpty && MultiQueryBoomerangTest.FAIL_ON_IMPRECISE) throw new Nothing(Joiner.on("\n").join(imprecisionErrors))
    }
  }

  private def compareQuery(query: Nothing, results: Nothing): Unit = {
    val expectedResults = expectedAllocsForQuery.get(query)
    val falseNegativeAllocationSites = new Nothing
    import scala.collection.JavaConversions._
    for (res <- expectedResults) {
      if (!results.contains(res)) falseNegativeAllocationSites.add(res)
    }
    val falsePositiveAllocationSites = new Nothing(results)
    import scala.collection.JavaConversions._
    for (res <- expectedResults) {
      falsePositiveAllocationSites.remove(res)
    }
    val answer = (if (falseNegativeAllocationSites.isEmpty) ""
    else "\nFN:" + falseNegativeAllocationSites) + (if (falsePositiveAllocationSites.isEmpty) ""
    else "\nFP:" + falsePositiveAllocationSites + "\n")
    if (!falseNegativeAllocationSites.isEmpty) unsoundErrors.add(new Nothing(" Unsound results for:" + answer))
    if (!falsePositiveAllocationSites.isEmpty) imprecisionErrors.add(new Nothing(" Imprecise results for:" + answer))
    import scala.collection.JavaConversions._
    for (e <- expectedAllocsForQuery.entries) {
      if (!e.getKey.equals(query)) if (results.contains(e.getValue)) throw new Nothing("A query contains the result of a different query.\n" + query + " \n contains \n" + e.getValue)
    }
  }

  private def runDemandDrivenBackward(): Unit = {
    val options = new Nothing() {
      @Override def analysisTimeoutMS: Int = analysisTimeout

      @Override def onTheFlyCallGraph = false

      @Override def allowMultipleQueries = true
    }
    solver = new Nothing(callGraph, dataFlowScope, options)
    import scala.collection.JavaConversions._
    for (query <- queryForCallSites) {
      if (query.isInstanceOf[Nothing]) {
        val res = solver.solve(query.asInstanceOf[Nothing])
        compareQuery(query, res.getAllocationSites.keySet)
      }
    }
    solver.unregisterAllListeners
  }

  private def allocatesObjectOfInterest(rightOp: Nothing, `type`: Nothing): Boolean = {
    val interfaceType = Scene.v.getSootClass(`type`)
    if (!interfaceType.isInterface) return false
    val allocatedType = rightOp.getBaseType
    Scene.v.getActiveHierarchy.getImplementersOf(interfaceType).contains(allocatedType.getSootClass)
  }

  protected def errorOnVisitMethod: Nothing = Lists.newLinkedList

  protected def includeJDK = true

  protected def queryForAndNotEmpty(variable: Nothing): Unit = {
  }

  protected def intQueryFor(variable: Int): Unit = {
  }

  /**
   * A call to this method flags the object as at the call statement as not reachable by the
   * analysis.
   */
  protected def unreachable(variable: Nothing): Unit = {
  }

  /** This method can be used in test cases to create branching. It is not optimized away. */
  protected def staticallyUnknown = true
}