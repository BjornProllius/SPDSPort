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
package wpds.interfaces

import java.util
import wpds.impl.NestedWeightedPAutomatons
import wpds.impl.NormalRule
import wpds.impl.PopRule
import wpds.impl.PushRule
import wpds.impl.Rule
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton

trait IPushdownSystem[N <: Location, D <: State, W <: Weight] {
  def addRule(rule: Nothing): Boolean

  def getStates: Nothing

  def getNormalRules: Nothing

  def getPopRules: Nothing

  def getPushRules: Nothing

  def getAllRules: Nothing

  def getRulesStarting(start: D, string: N): Nothing

  def getNormalRulesEnding(start: D, string: N): Nothing

  def getPushRulesEnding(start: D, string: N): Nothing

  def prestar(initialAutomaton: Nothing): Unit

  def poststar(initialAutomaton: Nothing): Unit

  def poststar(initialAutomaton: Nothing, summaries: Nothing): Unit

  def registerUpdateListener(listener: Nothing): Unit

  def unregisterAllListeners(): Unit
}