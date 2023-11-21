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

import wpds.impl.Weight.NoWeight
import wpds.interfaces.Location
import wpds.interfaces.State

class UPopRule[N <: Location, D <: State](s1: D, l1: N, s2: D) extends Nothing(s1, l1, s2, NoWeight.NO_WEIGHT_ONE) {
  @Override def toString: Nothing = "<" + s1 + ";" + l1 + ">-><" + s2 + ">"
}