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
package example

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.DefaultBoomerangOptions
import boomerang.Query
import boomerang.results.BackwardBoomerangResults
import boomerang.scene.AnalysisScope
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.wala.WALACallGraph
import com.google.common.collect.Lists
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.callgraph.AnalysisCacheImpl
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.CallGraphBuilder
import com.ibm.wala.ipa.callgraph.CallGraphBuilderCancelException
import com.ibm.wala.ipa.callgraph.Entrypoint
import com.ibm.wala.ipa.callgraph.IAnalysisCacheView
import com.ibm.wala.ipa.callgraph.impl.DefaultEntrypoint
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.cha.ClassHierarchyException
import com.ibm.wala.ipa.cha.ClassHierarchyFactory
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.types.MethodReference
import com.ibm.wala.util.config.AnalysisScopeReader
import com.ibm.wala.util.io.FileProvider
import java.io.IOException
import java.util
import java.util.Collections
import wpds.impl.Weight

object ExampleMain1 {
  @throws[CallGraphBuilderCancelException]
  @throws[IOException]
  @throws[ClassHierarchyException]
  def main(args: Nothing*): Unit = {
    val mainClass = "example.BoomerangExampleTarget1"
    val walaCallGraph = setupWALA(mainClass)
    performAnalysis(walaCallGraph)
  }

  @throws[CallGraphBuilderCancelException]
  @throws[IOException]
  @throws[ClassHierarchyException]
  private def setupWALA(mainClass: Nothing) = {
    val walaScope = AnalysisScopeReader.readJavaScope("testScope.txt", (new Nothing).getFile("exclusion.txt"), classOf[ExampleMain1].getClassLoader)
    val cha = ClassHierarchyFactory.make(walaScope)
    val testCaseClassName = mainClass.replace(".", "/").replace("class ", "")
    val ref = MethodReference.findOrCreate(ClassLoaderReference.Application, "L" + testCaseClassName, "main", "([Ljava/lang/String;)V")
    val method = cha.resolveMethod(ref)
    val singleton = new Nothing() {
      @Override def iterator: Nothing = {
        val list = Lists.newArrayList
        list.add(new Nothing(method, cha))
        val ret = list.iterator
        ret
      }
    }
    val options = new Nothing(walaScope, singleton)
    val cache = new Nothing
    val rtaBuilder = Util.makeRTABuilder(options, cache, cha, walaScope)
    val makeCallGraph = rtaBuilder.makeCallGraph(options, null)
    new Nothing(makeCallGraph, cha)
  }

  protected def performAnalysis(cg: Nothing): Unit = {
    val scope = new Nothing(cg) {
      @Override protected def generate(edge: Nothing): Nothing = {
        val statement = edge.getStart
        if (statement.toString.contains("queryFor") && statement.containsInvokeExpr) {
          val arg = statement.getInvokeExpr.getArg(0)
          return Collections.singleton(BackwardQuery.make(edge, arg))
        }
        Collections.emptySet
      }
    }
    // 1. Create a Boomerang solver.
    val solver = new Nothing(cg, DataFlowScope.INCLUDE_ALL, new Nothing)
    // 2. Submit a query to the solver.
    val seeds = scope.computeSeeds
    import scala.collection.JavaConversions._
    for (query <- seeds) {
      System.out.println("Solving query: " + query)
      val backwardQueryResults = solver.solve(query.asInstanceOf[Nothing])
      System.out.println("All allocation sites of the query variable are:")
      System.out.println(backwardQueryResults.getAllocationSites)
    }
  }
}