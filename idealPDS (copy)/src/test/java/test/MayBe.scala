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
import typestate.TransitionFunction
import typestate.finiteautomata.ITransition
import typestate.finiteautomata.State

class MayBe private[test](unit: Nothing, accessGraph: Nothing, state: Nothing) extends Nothing(unit, accessGraph, state) {
  def toString: Nothing = "Maybe " + super.toString

  @Override def computedResults(results: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (t <- results.values) {
      // if(t.equals(Transition.identity()))
      // continue;
      val s = t.to
      if (s != null) if (state eq InternalState.ACCEPTING) satisfied |= !s.isErrorState
      else if (state eq InternalState.ERROR) satisfied |= s.isErrorState
    }
  }
}