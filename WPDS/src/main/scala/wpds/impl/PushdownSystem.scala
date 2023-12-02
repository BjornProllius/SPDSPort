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

import wpds.interfaces.{Location, State}

class PushdownSystem[N <: Location, D <: State] 
  extends WeightedPushdownSystem[N, D, NoWeight] {

  override def addRule(rule: Rule[N, D, NoWeight]): Boolean = {
    rule match {
      case _: UNormalRule[_, _] | _: UPopRule[_, _] | _: UPushRule[_, _, _] => super.addRule(rule)
      case _ => throw new RuntimeException("Trying to add a weighted rule to an unweighted PDS!")
    }
  }
}
