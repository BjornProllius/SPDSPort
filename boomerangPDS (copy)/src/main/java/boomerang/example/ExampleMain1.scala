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
package boomerang.example

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.DefaultBoomerangOptions
import boomerang.Query
import boomerang.results.BackwardBoomerangResults
import boomerang.scene.AnalysisScope
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.SootCallGraph
import java.io.File
import java.util
import java.util.Collections
import soot.G
import soot.PackManager
import soot.Scene
import soot.SceneTransformer
import soot.SootClass
import soot.SootMethod
import soot.Transform
import soot.Transformer
import soot.options.Options
import wpds.impl.Weight

object ExampleMain1 {
  def main(args: Nothing*): Unit = {
    val sootClassPath = getSootClassPath
    val mainClass = "boomerang.example.BoomerangExampleTarget1"
    setupSoot(sootClassPath, mainClass)
    analyze()
  }

  private def getSootClassPath = {
    // Assume target folder to be directly in user dir; this should work in eclipse
    var sootClassPath = System.getProperty("user.dir") + File.separator + "target" + File.separator + "classes"
    var classPathDir = new Nothing(sootClassPath)
    if (!classPathDir.exists) {
      // We haven't found our target folder
      // Check if if it is in the boomerangPDS in user dir; this should work in IntelliJ
      sootClassPath = System.getProperty("user.dir") + File.separator + "boomerangPDS" + File.separator + "target" + File.separator + "classes"
      classPathDir = new Nothing(sootClassPath)
      if (!classPathDir.exists) {
        // We haven't found our bytecode anyway, notify now instead of starting analysis anyway
        throw new Nothing("Classpath could not be found.")
      }
    }
    sootClassPath
  }

  private def setupSoot(sootClassPath: Nothing, mainClass: Nothing): Unit = {
    G.v.reset
    Options.v.set_whole_program(true)
    Options.v.setPhaseOption("cg.spark", "on")
    Options.v.set_output_format(Options.output_format_none)
    Options.v.set_no_bodies_for_excluded(true)
    Options.v.set_allow_phantom_refs(true)
    val includeList = new Nothing
    includeList.add("java.lang.*")
    includeList.add("java.util.*")
    includeList.add("java.io.*")
    includeList.add("sun.misc.*")
    includeList.add("java.net.*")
    includeList.add("javax.servlet.*")
    includeList.add("javax.crypto.*")
    Options.v.set_include(includeList)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.set_soot_classpath(sootClassPath)
    Options.v.set_prepend_classpath(true)
    // Options.v().set_main_class(this.getTargetClass());
    Scene.v.loadNecessaryClasses
    val c = Scene.v.forceResolve(mainClass, SootClass.BODIES)
    if (c != null) c.setApplicationClass
    import scala.collection.JavaConversions._
    for (m <- c.getMethods) {
      System.out.println(m)
    }
  }

  private def analyze(): Unit = {
    val transform = new Nothing("wjtp.ifds", createAnalysisTransformer)
    PackManager.v.getPack("wjtp").add(transform)
    PackManager.v.getPack("cg").apply
    BoomerangPretransformer.v.apply
    PackManager.v.getPack("wjtp").apply
  }

  private def createAnalysisTransformer = new Nothing() {
    protected def internalTransform(phaseName: Nothing, @SuppressWarnings("rawtypes") options: Nothing): Unit = {
      val sootCallGraph = new Nothing
      val scope = new Nothing(sootCallGraph) {
        @Override protected def generate(cfgEdge: Nothing): Nothing = {
          val statement = cfgEdge.getTarget
          if (statement.toString.contains("queryFor") && statement.containsInvokeExpr) {
            val arg = statement.getInvokeExpr.getArg(0)
            return Collections.singleton(BackwardQuery.make(cfgEdge, arg))
          }
          Collections.emptySet
        }
      }
      // 1. Create a Boomerang solver.
      val solver = new Nothing(sootCallGraph, SootDataFlowScope.make(Scene.v), new Nothing)
      // 2. Submit a query to the solver.
      val seeds = scope.computeSeeds
      import scala.collection.JavaConversions._
      for (query <- seeds) {
        System.out.println("Solving query: " + query)
        val backwardQueryResults = solver.solve(query.asInstanceOf[Nothing])
        System.out.println("All allocation sites of the query variable are:")
        System.out.println(backwardQueryResults.getAllocationSites)
        System.out.println("All aliasing access path of the query variable are:")
        System.out.println(backwardQueryResults.getAllAliases)
      }
    }
  }
}