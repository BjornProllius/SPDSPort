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
package wpds.impl

import scala.collection.mutable
import wpds.interfaces.{Location, State}

class SummaryNestedWeightedPAutomatons[N <: Location, D <: State, W <: Weight] extends NestedWeightedPAutomatons[N, D, W] {

    private val summaries: mutable.Map[D, WeightedPAutomaton[N, D, W]] = mutable.Map()

    override def putSummaryAutomaton(target: D, aut: WeightedPAutomaton[N, D, W]): Unit = {
        summaries.put(target, aut)
    }

    override def getSummaryAutomaton(target: D): WeightedPAutomaton[N, D, W] = {
        summaries(target)
    }
}