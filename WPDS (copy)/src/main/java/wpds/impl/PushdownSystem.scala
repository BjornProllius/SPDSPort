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

class PushdownSystem[N <: Location, D <: State] extends Nothing {
  @Override def addRule(rule: Nothing): Boolean = {
    if (!rule.isInstanceOf[Nothing] && !rule.isInstanceOf[Nothing] && !rule.isInstanceOf[Nothing]) throw new Nothing("Trying to add a weighted rule to an unweighted PDS!")
    super.addRule(rule)
  }
}