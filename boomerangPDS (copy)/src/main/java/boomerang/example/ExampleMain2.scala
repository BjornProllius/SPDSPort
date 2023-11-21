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

import boomerang.Boomerang
import boomerang.DefaultBoomerangOptions
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.AllocVal
import boomerang.scene.AnalysisScope
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.SootCallGraph
import com.google.common.collect.Table
import java.io.File
import java.util
import java.util.Collections
import soot.G
import soot.PackManager
import soot.Scene
import soot.SceneTransformer
import soot.SootClass
import soot.Transform
import soot.Transformer
import soot.options.Options
import wpds.impl.Weight.NoWeight

object ExampleMain2 {
  def main(args: Nothing*): Unit = {
    val sootClassPath = getSootClassPath
    val mainClass = "boomerang.example.BoomerangExampleTarget2"
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
    var c = Scene.v.forceResolve(mainClass, SootClass.BODIES)
    if (c != null) c.setApplicationClass
    // Force resolve inner classes, as the setup does currently not load them automatically.
    c = Scene.v.forceResolve(mainClass + "$NestedClassWithField", SootClass.BODIES)
    c.setApplicationClass
    c = Scene.v.forceResolve(mainClass + "$ClassWithField", SootClass.BODIES)
    c.setApplicationClass
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
          val statement = cfgEdge.getStart
          if (statement.getMethod.toString.contains("ClassWithField") && statement.getMethod.isConstructor && statement.isAssign) if (statement.getRightOp.isIntConstant) return Collections.singleton(new Nothing(cfgEdge, new Nothing(statement.getLeftOp, statement, statement.getRightOp)))
          Collections.emptySet
        }
      }
      val seeds = scope.computeSeeds
      import scala.collection.JavaConversions._
      for (query <- seeds) {
        // 1. Create a Boomerang solver.
        val solver = new Nothing(sootCallGraph, SootDataFlowScope.make(Scene.v), new Nothing)
        System.out.println("Solving query: " + query)
        // 2. Submit a query to the solver.
        val forwardBoomerangResults = solver.solve(query.asInstanceOf[Nothing])
        // 3. Process forward results
        val results = forwardBoomerangResults.asStatementValWeightTable
        import scala.collection.JavaConversions._
        for (s <- results.rowKeySet) {
          // 4. Filter results based on your use statement, in our case the call of
          // System.out.println(n.nested.field)
          if (s.getTarget.toString.contains("println")) {
            // 5. Check that a propagated value is used at the particular statement.
            import scala.collection.JavaConversions._
            for (reachingVal <- results.row(s).keySet) {
              if (s.getTarget.uses(reachingVal)) System.out.println(query + " reaches " + s)
            }
          }
        }
      }
    }
  }
}