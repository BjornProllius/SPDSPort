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
package ideal

import boomerang.ForwardQuery
import boomerang.Query
import boomerang.WeightedForwardQuery
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.AnalysisScope
import boomerang.scene.ControlFlowGraph.Edge
import com.google.common.base.Stopwatch
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import typestate.TransitionFunction
import wpds.impl.Weight


object IDEALAnalysis {
  private val LOGGER = LoggerFactory.getLogger(classOf[IDEALAnalysis[_ <: Nothing]])
  var PRINT_OPTIONS = false
}

class IDEALAnalysis[W <: Weight](protected val analysisDefinition: Nothing) {
  this.seedFactory = new Nothing(analysisDefinition.callGraph) {
    @Override protected def generate(stmt: Nothing): Nothing = analysisDefinition.generate(stmt)
  }
  final private var seedFactory: Nothing = null
  private var seedCount = 0
  private val analysisTime = new Nothing
  private val timedoutSeeds = new Nothing

  def run(): Unit = {
    printOptions()
    val initialSeeds = seedFactory.computeSeeds
    if (initialSeeds.isEmpty) IDEALAnalysis.LOGGER.info("No seeds found!")
    else IDEALAnalysis.LOGGER.info("Analysing {} seeds!", initialSeeds.size)
    import scala.collection.JavaConversions._
    for (s <- initialSeeds) {
      if (!s.isInstanceOf[Nothing]) continue //todo: continue is not supported
      val seed = s.asInstanceOf[Nothing]
      seedCount += 1
      IDEALAnalysis.LOGGER.info("Analyzing {}", seed)
      val watch = Stopwatch.createStarted
      analysisTime.put(seed, watch)
      run(seed)
      watch.stop
      IDEALAnalysis.LOGGER.debug("Analyzed (finished,timedout): \t ({},{}) of {} seeds", seedCount - timedoutSeeds.size, timedoutSeeds.size, initialSeeds.size)
    }
  }

  def run(seed: Nothing): Nothing = {
    val idealAnalysis = new Nothing(analysisDefinition, seed)
    var res: Nothing = null
    try res = idealAnalysis.run
    catch {
      case e: Nothing =>
        res = e.getLastResults.asInstanceOf[Nothing]
        timedoutSeeds.add(seed.asInstanceOf[Nothing])
    }
    analysisDefinition.getResultHandler.report(seed.asInstanceOf[Nothing], res)
    res
  }

  private def printOptions(): Unit = {
    if (IDEALAnalysis.PRINT_OPTIONS) System.out.println(analysisDefinition)
  }

  def computeSeeds: Nothing = seedFactory.computeSeeds

  def getAnalysisTime(key: Nothing): Nothing = analysisTime.get(key)

  def isTimedout(key: Nothing): Boolean = timedoutSeeds.contains(key)
}
