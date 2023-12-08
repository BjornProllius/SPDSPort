import boomerang.{ForwardQuery, Query}
import boomerang.scene.ControlFlowGraph.Edge
import com.google.common.collect.{Lists, Sets}

import scala.collection.mutable

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
abstract class PointOfIndirection[Statement, Val, Field] {

  private val actualBaseAllocations: mutable.Set[ForwardQuery] = Sets.newHashSet()
  private val flowAllocations: mutable.Set[Query] = Sets.newHashSet()

  def execute(baseAllocation: ForwardQuery, flowAllocation: Query): Unit

  def addBaseAllocation(baseAllocation: ForwardQuery): Unit = {
    if (actualBaseAllocations.add(baseAllocation)) {
      for (flowAllocation <- Lists.newArrayList(flowAllocations)) {
        execute(baseAllocation, flowAllocation)
      }
    }
  }

  def addFlowAllocation(flowAllocation: Query): Unit = {
    if (flowAllocations.add(flowAllocation)) {
      for (baseAllocation <- Lists.newArrayList(actualBaseAllocations)) {
        execute(baseAllocation, flowAllocation)
      }
    }
  }

  def getCfgEdge: Edge
}