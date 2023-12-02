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
package inference.example

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable
import boomerang.{WeightedForwardQuery, WeightedTransformer}
import boomerang.debugger.Debugger
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.{CallGraph, ControlFlowGraph, DataFlowScope, Statement, Val}
import boomerang.scene.jimple.{BoomerangPretransformer, SootCallGraph}
import com.google.common.base.Joiner
import com.google.common.collect.Table
import ideal._
import sync.pds.solver.WeightFunctions
import org.slf4j.LoggerFactory
import soot.{G, Options, PackManager, Scene, SootClass, SootMethod, Transform, Transformer}

object Main {
  private val logger = LoggerFactory.getLogger(classOf[Main])

  def main(args: String*): Unit = {
    val sootClassPath = System.getProperty("user.dir") + File.separator + "target" + File.separator + "classes"
    val mainClass = "inference.example.InferenceExample"
    setupSoot(sootClassPath, mainClass)
    analyze()
  }

  private def setupSoot(sootClassPath: String, mainClass: String): Unit = {
    G.v().reset()
    Options.v().set_whole_program(true)
    Options.v().setPhaseOption("cg.spark", "on")
    Options.v().set_output_format(Options.output_format_none)
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)

    val includeList = List(
      "java.lang.*",
      "java.util.*",
      "java.io.*",
      "sun.misc.*",
      "java.net.*",
      "javax.servlet.*",
      "javax.crypto.*"
    )

    Options.v().set_include(includeList.asJava)
    Options.v().setPhaseOption("jb", "use-original-names:true")

    Options.v().set_soot_classpath(sootClassPath)
    Options.v().set_prepend_classpath(true)

    Scene.v().loadNecessaryClasses()
    val c = Scene.v().forceResolve(mainClass, SootClass.BODIES)
    if (c != null) {
      c.setApplicationClass()
      c.getMethods.asScala.foreach(m => logger.debug(m.toString))
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
      def internalTransform(phaseName: String, options: java.util.Map[_, _]): Unit = {
        val resultHandler = new StoreIDEALResultHandler[InferenceWeight]()
        val callGraph = new SootCallGraph()
        val solver = new IDEALAnalysis[InferenceWeight](new IDEALAnalysisDefinition[InferenceWeight] {
          def generate(edge: ControlFlowGraph.Edge): Collection[WeightedForwardQuery[InferenceWeight]] = {
            val stmt = edge.getStart
            if (stmt.isAssign && stmt.getRightOp.isNewExpr &&
              stmt.getRightOp.getType.toString.contains("inference.example.InferenceExample$File")) {
              Collections.singleton(
                new WeightedForwardQuery[InferenceWeight](edge, stmt.getLeftOp, InferenceWeight.one())
              )
            } else {
              Collections.emptySet()
            }
          }

          def weightFunctions(): WeightFunctions[ControlFlowGraph.Edge, Val, ControlFlowGraph.Edge, InferenceWeight] =
            new InferenceWeightFunctions()

          def debugger(solver: IDEALSeedSolver[InferenceWeight]): Debugger[InferenceWeight] =
            new Debugger[InferenceWeight]()

          def getResultHandler: IDEALResultHandler[InferenceWeight] = resultHandler

          def callGraph(): CallGraph = callGraph

          protected def getDataFlowScope: DataFlowScope = SootDataFlowScope.make(Scene.v())
        })
        solver.run()
        val res: mutable.Map[WeightedForwardQuery[InferenceWeight], ForwardBoomerangResults[InferenceWeight]] =
          resultHandler.getResults.asScala
        for ((e, results) <- res) {
          val table: Table[ControlFlowGraph.Edge, Val, InferenceWeight] = results.asStatementValWeightTable()
          logger.info(Joiner.on("\n").join(table.cellSet()))
        }
      }
    }
  }
}
