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

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import ideal.IDEALSeedSolver.Phases
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.PushNode
import wpds.impl.Weight

object IDEALWeightFunctions {
  private val logger = LoggerFactory.getLogger(classOf[IDEALWeightFunctions[_ <: Nothing]])
}

class IDEALWeightFunctions[W <: Weight](private var delegate: Nothing, private var strongUpdates: Boolean) extends Nothing {
  private val listeners = Sets.newHashSet
  private val potentialStrongUpdates = Sets.newHashSet
  private val weakUpdates = Sets.newHashSet
  private val nonOneFlowNodes = Sets.newHashSet
  private var phase: Nothing = null
  private val indirectAlias = HashMultimap.create
  private val nodesWithStrongUpdate = Sets.newHashSet

  @Override def push(curr: Nothing, succ: Nothing, calleeSp: Nothing): W = {
    val weight = delegate.push(curr, succ, calleeSp)
    if (isObjectFlowPhase && !weight.equals(getOne)) if (succ.isInstanceOf[Nothing]) {
      val pushNode = succ.asInstanceOf[Nothing]
      addOtherThanOneWeight(new Nothing(pushNode.location, curr.fact))
    }
    weight
  }

  private[ideal] def addOtherThanOneWeight(curr: Nothing): Unit = {
    if (nonOneFlowNodes.add(curr)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(listeners)) {
        l.nonOneFlow(curr)
      }
    }
  }

  @Override def normal(curr: Nothing, succ: Nothing): W = {
    val weight = delegate.normal(curr, succ)
    if (isObjectFlowPhase && succ.stmt.getTarget.containsInvokeExpr && !weight.equals(getOne)) addOtherThanOneWeight(succ)
    weight
  }

  private def isObjectFlowPhase = phase.equals(Phases.ObjectFlow)

  private def isValueFlowPhase = phase.equals(Phases.ValueFlow)

  @Override def pop(curr: Nothing): W = delegate.pop(curr)

  def registerListener(listener: Nothing): Unit = {
    if (listeners.add(listener)) {
      import scala.collection.JavaConversions._
      for (existing <- Lists.newArrayList(nonOneFlowNodes)) {
        listener.nonOneFlow(existing)
      }
    }
  }

  @Override def getOne: W = delegate.getOne

  @Override def toString: Nothing = "[IDEAL-Wrapped Weights] " + delegate.toString

  def potentialStrongUpdate(stmt: Nothing): Unit = {
    potentialStrongUpdates.add(stmt)
  }

  def weakUpdate(stmt: Nothing): Unit = {
    weakUpdates.add(stmt)
  }

  def setPhase(phase: Nothing): Unit = {
    this.phase = phase
  }

  def addIndirectFlow(source: Nothing, target: Nothing): Unit = {
    if (source.equals(target)) return
    IDEALWeightFunctions.logger.trace("Alias flow detected " + source + " " + target)
    indirectAlias.put(source, target)
  }

  def getAliasesFor(node: Nothing): Nothing = indirectAlias.get(node)

  def isStrongUpdateStatement(stmt: Nothing): Boolean = potentialStrongUpdates.contains(stmt) && !weakUpdates.contains(stmt) && strongUpdates

  def isKillFlow(node: Nothing): Boolean = !nodesWithStrongUpdate.contains(node)

  def addNonKillFlow(curr: Nothing): Unit = {
    nodesWithStrongUpdate.add(curr)
  }
}