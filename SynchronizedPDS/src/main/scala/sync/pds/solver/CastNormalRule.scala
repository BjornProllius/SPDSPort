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
package sync.pds.solver

import wpds.impl.NormalRule
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.interfaces.Location
import wpds.interfaces.State

class CastNormalRule[N <: Location, D <: State, W <: Weight](s1: D, l1: N, s2: D, l2: N, w: W) extends Nothing(s1, l1, s2, l2, w) {
  @Override def canBeApplied(t: Nothing, weight: W): Boolean = super.canBeApplied(t, weight)
}