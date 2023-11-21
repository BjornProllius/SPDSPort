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
package boomerang.poi

import boomerang.ForwardQuery
import boomerang.Query
import boomerang.scene.ControlFlowGraph.Edge
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util

abstract class PointOfIndirection[Statement, Val, Field] {
  private val actualBaseAllocations = Sets.newHashSet
  private val flowAllocations = Sets.newHashSet

  def execute(baseAllocation: Nothing, flowAllocation: Nothing): Unit

  def addBaseAllocation(baseAllocation: Nothing): Unit = {
    if (actualBaseAllocations.add(baseAllocation)) {
      import scala.collection.JavaConversions._
      for (flowAllocation <- Lists.newArrayList(flowAllocations)) {
        execute(baseAllocation, flowAllocation)
      }
    }
  }

  def addFlowAllocation(flowAllocation: Nothing): Unit = {
    if (flowAllocations.add(flowAllocation)) {
      import scala.collection.JavaConversions._
      for (baseAllocation <- Lists.newArrayList(actualBaseAllocations)) {
        execute(baseAllocation, flowAllocation)
      }
    }
  }

  def getCfgEdge: Nothing
}