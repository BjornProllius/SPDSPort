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
import boomerang.{Boomerang, DefaultBoomerangOptions, ForwardQuery, Query}
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.{AllocVal, AnalysisScope, Statement, Val}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.SootDataFlowScope
import boomerang.scene.jimple.{BoomerangPretransformer, SootCallGraph}
import com.google.common.collect.Table
import java.io.File
import java.util.{Collection, Collections, LinkedList, List, Map}
import soot.{G, PackManager, Scene, SceneTransformer, SootClass, Transform, Transformer}
import soot.options.Options
import wpds.impl.Weight.NoWeight

object ExampleMain2 {
  def main(args: Array[String]): Unit = {
    val sootClassPath = getSootClassPath()
    val mainClass = "boomerang.example.BoomerangExampleTarget2"
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
    var c = Scene.v().forceResolve(mainClass, SootClass.BODIES)
    if (c != null) {
      c.setApplicationClass()
    }
    // Force resolve inner classes, as the setup does currently not load them automatically.
    c = Scene.v().forceResolve(mainClass + "$NestedClassWithField", SootClass.BODIES)
    c.setApplicationClass()

    c = Scene.v().forceResolve(mainClass + "$ClassWithField", SootClass.BODIES)
    c.setApplicationClass()
  }

  private def analyze(): Unit = {
    val transform = new Transform("wjtp.ifds", createAnalysisTransformer())
    PackManager.v().getPack("wjtp").add(transform)
    PackManager.v().getPack("cg").apply()
    BoomerangPretransformer.v().apply()
    PackManager.v().getPack("wjtp").apply()
  }
  
  private def createAnalysisTransformer(): Transformer = {
    new SceneTransformer() {
      protected def internalTransform(phaseName: String, options: Map[String, String]): Unit = {
        val sootCallGraph = new SootCallGraph()
        val scope = new AnalysisScope(sootCallGraph) {
          override protected def generate(cfgEdge: Edge): Collection[Query] = {
            val statement = cfgEdge.getStart()
            if (statement.getMethod().toString().contains("ClassWithField")
                && statement.getMethod().isConstructor()
                && statement.isAssign()) {
              if (statement.getRightOp().isIntConstant()) {
                return Collections.singleton(
                  new ForwardQuery(
                    cfgEdge,
                    new AllocVal(
                      statement.getLeftOp(), statement, statement.getRightOp())))
              }
            }
            Collections.emptySet()
          }
        }
  
        val seeds = scope.computeSeeds()
        for (query <- seeds) {
          // 1. Create a Boomerang solver.
          val solver = new Boomerang(
            sootCallGraph, SootDataFlowScope.make(Scene.v()), new DefaultBoomerangOptions())
          println("Solving query: " + query)
          // 2. Submit a query to the solver.
          val forwardBoomerangResults = solver.solve(query.asInstanceOf[ForwardQuery])
  
          // 3. Process forward results
          val results = forwardBoomerangResults.asStatementValWeightTable()
          for (s <- results.rowKeySet()) {
            // 4. Filter results based on your use statement, in our case the call of
            // System.out.println(n.nested.field)
            if (s.getTarget().toString().contains("println")) {
              // 5. Check that a propagated value is used at the particular statement.
              for (reachingVal <- results.row(s).keySet()) {
                if (s.getTarget().uses(reachingVal)) {
                  println(query + " reaches " + s)
                }
              }
            }
          }
        }
      }
    }
  }
}
