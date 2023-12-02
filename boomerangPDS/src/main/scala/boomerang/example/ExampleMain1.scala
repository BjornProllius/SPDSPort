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
import boomerang.{BackwardQuery, Boomerang, DefaultBoomerangOptions, Query}
import boomerang.results.BackwardBoomerangResults
import boomerang.scene.{AnalysisScope, Edge, Statement, Val, SootDataFlowScope}
import boomerang.scene.jimple.{BoomerangPretransformer, SootCallGraph}
import java.io.File
import java.util.{Collection, Collections, LinkedList, List, Map}
import soot.{G, PackManager, Scene, SceneTransformer, SootClass, SootMethod, Transform, Transformer}
import soot.options.Options
import wpds.impl.Weight

object ExampleMain1 {
  def main(args: Array[String]): Unit = {
    val sootClassPath = getSootClassPath()
    val mainClass = "boomerang.example.BoomerangExampleTarget1"
    setupSoot(sootClassPath, mainClass)
    analyze()
  }

  private def getSootClassPath(): String = {
    // Assume target folder to be directly in user dir; this should work in eclipse
    var sootClassPath =
      System.getProperty("user.dir") + File.separator + "target" + File.separator + "classes"
    var classPathDir = new File(sootClassPath)
    if (!classPathDir.exists()) {
      // We haven't found our target folder
      // Check if if it is in the boomerangPDS in user dir; this should work in IntelliJ
      sootClassPath =
        System.getProperty("user.dir") +
          File.separator +
          "boomerangPDS" +
          File.separator +
          "target" +
          File.separator +
          "classes"
      classPathDir = new File(sootClassPath)
      if (!classPathDir.exists()) {
        // We haven't found our bytecode anyway, notify now instead of starting analysis anyway
        throw new RuntimeException("Classpath could not be found.")
      }
    }
    sootClassPath
  }

  private def setupSoot(sootClassPath: String, mainClass: String): Unit = {
    G.v().reset()
    Options.v().set_whole_program(true)
    Options.v().setPhaseOption("cg.spark", "on")
    Options.v().set_output_format(Options.output_format_none)
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)

    val includeList = new LinkedList[String]()
    includeList.add("java.lang.*")
    includeList.add("java.util.*")
    includeList.add("java.io.*")
    includeList.add("sun.misc.*")
    includeList.add("java.net.*")
    includeList.add("javax.servlet.*")
    includeList.add("javax.crypto.*")

    Options.v().set_include(includeList)
    Options.v().setPhaseOption("jb", "use-original-names:true")

    Options.v().set_soot_classpath(sootClassPath)
    Options.v().set_prepend_classpath(true)
    // Options.v().set_main_class(this.getTargetClass());
    Scene.v().loadNecessaryClasses()
    val c = Scene.v().forceResolve(mainClass, SootClass.BODIES)
    if (c != null) {
      c.setApplicationClass()
    }
    for (m <- c.getMethods) {
      println(m)
    }
  }
  
  private def analyze(): Unit = {
    val transform = new Transform("wjtp.ifds", createAnalysisTransformer())
    PackManager.v().getPack("wjtp").add(transform)
    PackManager.v().getPack("cg").apply()
    BoomerangPretransformer.v().apply()
    PackManager.v().getPack("wjtp").apply()
  }
  
  private def createAnalysisTransformer(): Transformer = {
    new Transformer {
      protected def internalTransform(phaseName: String, options: Map[_, _]): Unit = {
        val sootCallGraph = new SootCallGraph()
        val scope = new AnalysisScope(sootCallGraph) {
          override protected def generate(cfgEdge: Edge): Collection[_ <: Query] = {
            val statement = cfgEdge.getTarget()
            if (statement.toString().contains("queryFor") && statement.containsInvokeExpr()) {
              val arg = statement.getInvokeExpr().getArg(0)
              Collections.singleton(BackwardQuery.make(cfgEdge, arg))
            } else {
              Collections.emptySet()
            }
          }
        }
  
        // 1. Create a Boomerang solver.
        val solver = new Boomerang(sootCallGraph, SootDataFlowScope.make(Scene.v()), new DefaultBoomerangOptions())
  
        // 2. Submit a query to the solver.
        val seeds = scope.computeSeeds()
        for (query <- seeds) {
          println("Solving query: " + query)
          val backwardQueryResults = solver.solve(query.asInstanceOf[BackwardQuery])
          println("All allocation sites of the query variable are:")
          println(backwardQueryResults.getAllocationSites())
  
          println("All aliasing access path of the query variable are:")
          println(backwardQueryResults.getAllAliases())
        }
      }
    }
  }
}
