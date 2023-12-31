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

trait WPAUpdateListener[N <: Location, D <: State, W <: Weight] {
    def onWeightAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit
}