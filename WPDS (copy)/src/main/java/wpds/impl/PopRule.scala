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

import wpds.interfaces.Location
import wpds.interfaces.State

class PopRule[N <: Location, D <: State, W <: Weight](s1: D, l1: N, s2: D, w: W) extends Nothing(s1, l1, s2, null, w) {
  @Override def toString: Nothing = "<" + s1 + ";" + l1 + ">-><" + s2 + ">(" + w + ")"
}