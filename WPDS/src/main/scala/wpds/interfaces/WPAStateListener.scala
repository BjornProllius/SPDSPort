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

import wpds.impl.{Transition, Weight, WeightedPAutomaton}

abstract class WPAStateListener[N <: Location, D <: State, W <: Weight](protected val state: D) {

    def onOutTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit

    def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit

    def getState: D = state

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + (if (state == null) 0 else state.hashCode)
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: WPAStateListener[N, D, W] =>
            (this eq that) || (that canEqual this) && (state == null && that.state == null || state != null && state == that.state)
        case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[WPAStateListener[N, D, W]]
}