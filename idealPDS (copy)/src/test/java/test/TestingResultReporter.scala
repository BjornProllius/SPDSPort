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

import boomerang.results.ForwardBoomerangResults
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import boomerang.scene.Val
import com.google.common.collect.HashBasedTable
import com.google.common.collect.HashMultimap
import com.google.common.collect.Multimap
import com.google.common.collect.Table
import com.google.common.collect.Table.Cell
import java.util.Map.Entry
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

class TestingResultReporter[W <: Weight](expectedResults: Nothing) {

  import scala.collection.JavaConversions._

  for (e <- expectedResults) {
    if (e.isInstanceOf[Nothing]) stmtToResults.put(e.asInstanceOf[Nothing].getStmt, e)
  }
  private val stmtToResults = HashMultimap.create

  def onSeedFinished(seed: Nothing, res: Nothing): Unit = {
    val resultsAsCFGEdges = res.asStatementValWeightTable
    val results = HashBasedTable.create
    import scala.collection.JavaConversions._
    for (c <- resultsAsCFGEdges.cellSet) {
      results.put(c.getRowKey.getTarget, c.getColumnKey, c.getValue)
    }
    import scala.collection.JavaConversions._
    for (e <- stmtToResults.entries) {
      if (e.getValue.isInstanceOf[Nothing]) {
        val expectedResults = e.getValue.asInstanceOf[Nothing]
        val w2 = results.get(e.getKey, expectedResults.getVal)
        if (w2 != null) expectedResults.computedResults(w2)
      }
      // check if any of the methods that should not be analyzed have been analyzed
      if (e.getValue.isInstanceOf[Nothing]) {
        val shouldNotBeAnalyzed = e.getValue.asInstanceOf[Nothing]
        val analyzedUnit = e.getKey
        if (analyzedUnit.equals(shouldNotBeAnalyzed.unit)) shouldNotBeAnalyzed.hasBeenAnalyzed
      }
    }
  }
}