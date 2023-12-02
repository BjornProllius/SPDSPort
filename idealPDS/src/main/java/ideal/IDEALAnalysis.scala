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

  import boomerang.{ForwardQuery, WeightedForwardQuery, ForwardBoomerangResults}
  import boomerang.scene.{AnalysisScope, ControlFlowGraph}
  import com.google.common.base.Stopwatch
  import org.slf4j.{Logger, LoggerFactory}
  import typestate.TransitionFunction
  import wpds.impl.Weight
  
  import scala.collection.mutable
  import scala.collection.JavaConverters._
  
  class IDEALAnalysis[W <: Weight](val analysisDefinition: IDEALAnalysisDefinition[W]) {
  
    import IDEALAnalysis._
  
    private val seedFactory: AnalysisScope = new AnalysisScope(analysisDefinition.callGraph()) {
      override protected def generate(stmt: ControlFlowGraph.Edge): Collection[WeightedForwardQuery[W]] =
        analysisDefinition.generate(stmt).asJava
    }
  
    private var seedCount: Int = 0
    private val analysisTime: mutable.Map[WeightedForwardQuery[W], Stopwatch] = mutable.HashMap()
    private val timedoutSeeds: mutable.Set[WeightedForwardQuery[W]] = mutable.HashSet()
  
    def run(): Unit = {
      printOptions()
  
      val initialSeeds: Collection[ForwardQuery] = seedFactory.computeSeeds()
  
      if (initialSeeds.isEmpty) LOGGER.info("No seeds found!")
      else LOGGER.info("Analyzing {} seeds!", initialSeeds.size())
  
      for (s <- initialSeeds.asScala) {
        s match {
          case seed: WeightedForwardQuery[W] =>
            seedCount += 1
            LOGGER.info("Analyzing {}", seed)
            val watch = Stopwatch.createStarted()
            analysisTime.put(seed, watch)
            run(seed)
            watch.stop()
            LOGGER.debug(
              "Analyzed (finished,timedout): \t ({},{}) of {} seeds",
              seedCount - timedoutSeeds.size,
              timedoutSeeds.size,
              initialSeeds.size()
            )
          case _ =>
        }
      }
    }
  
    def run(seed: ForwardQuery): ForwardBoomerangResults[W] = {
      val idealAnalysis = new IDEALSeedSolver[W](analysisDefinition, seed)
      val res: ForwardBoomerangResults[W] =
        try {
          idealAnalysis.run()
        } catch {
          case e: IDEALSeedTimeout =>
            res = e.getLastResults.asInstanceOf[ForwardBoomerangResults[W]]
            timedoutSeeds.add(seed.asInstanceOf[WeightedForwardQuery[W]])
            res
        }
      analysisDefinition.getResultHandler.report(seed.asInstanceOf[WeightedForwardQuery[W]], res)
      res
    }
  
    private def printOptions(): Unit = {
      if (PRINT_OPTIONS) {
        println(analysisDefinition)
      }
    }
  
    def computeSeeds(): Collection[ForwardQuery] = seedFactory.computeSeeds()
  
    def getAnalysisTime(key: WeightedForwardQuery[TransitionFunction]): Stopwatch = analysisTime(key)
  
    def isTimedout(key: WeightedForwardQuery[TransitionFunction]): Boolean = timedoutSeeds.contains(key)
  }
  
  object IDEALAnalysis {
    private val LOGGER: Logger = LoggerFactory.getLogger(classOf[IDEALAnalysis[_]])
    var PRINT_OPTIONS: Boolean = false
  }
  