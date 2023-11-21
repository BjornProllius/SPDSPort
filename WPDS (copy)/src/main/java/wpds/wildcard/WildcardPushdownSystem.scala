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
package wpds.wildcard

import java.util
import wpds.impl.NormalRule
import wpds.impl.PopRule
import wpds.impl.PushRule
import wpds.impl.PushdownSystem
import wpds.impl.Rule
import wpds.impl.UNormalRule
import wpds.impl.UPopRule
import wpds.impl.UPushRule
import wpds.impl.Weight.NoWeight
import wpds.interfaces.Location
import wpds.interfaces.State

abstract class WildcardPushdownSystem[N <: Location, D <: State] extends Nothing {
  @Override def getRulesStarting(start: D, string: N): Nothing = {
    assert(!string.equals(anyTransition))
    val allRules = getAllRules
    val result = new Nothing
    import scala.collection.JavaConversions._
    for (r <- allRules) {
      if (r.getS1.equals(start) && r.getL1.equals(string)) result.add(r)
      if (anyTransition != null && r.getS1.equals(start) && r.getL1.equals(anyTransition)) if (r.isInstanceOf[Nothing]) result.add(new Nothing(r.getS1, string, r.getS2, string))
      else if (r.isInstanceOf[Nothing]) result.add(new Nothing(r.getS1, string, r.getS2))
      else if (r.isInstanceOf[Nothing]) result.add(new Nothing(r.getS1, string, r.getS2, r.getL2, string))
    }
    result
  }

  @Override def getNormalRulesEnding(start: D, string: N): Nothing = {
    assert(!string.equals(anyTransition))
    val allRules = getNormalRules
    val result = new Nothing
    import scala.collection.JavaConversions._
    for (r <- allRules) {
      if (r.getS2.equals(start) && r.getL2.equals(string)) result.add(r)
      if (r.getS2.equals(start) && r.getL2.equals(anyTransition)) result.add(new Nothing(r.getS1, string, r.getS2, string))
    }
    result
  }

  @Override def getPushRulesEnding(start: D, string: N): Nothing = {
    assert(!string.equals(anyTransition))
    val allRules = getPushRules
    val result = new Nothing
    import scala.collection.JavaConversions._
    for (r <- allRules) {
      if (r.getS2.equals(start) && r.getL2.equals(string)) result.add(r)
    }
    result
  }

  def anyTransition: Nothing
}