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
package test

import boomerang.scene.Statement
import boomerang.scene.Val
import com.google.common.collect.Sets
import java.util
import typestate.TransitionFunction
import typestate.finiteautomata.ITransition
import typestate.finiteautomata.State
import typestate.finiteautomata.Transition

class MustBe private[test](unit: Nothing, `val`: Nothing, state: Nothing) extends Nothing(unit, `val`, state) {
  def toString: Nothing = "MustBe " + super.toString

  @Override def computedResults(`val`: Nothing): Unit = {
    val states = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (t <- `val`.values) {
      if (!t.equals(Transition.identity)) states.add(t.to)
    }
    import scala.collection.JavaConversions._
    for (s <- states) {
      if (state eq InternalState.ACCEPTING) satisfied |= !s.isErrorState && (states.size eq 1)
      else if (state eq InternalState.ERROR) satisfied |= s.isErrorState && (states.size eq 1)
    }
  }
}