/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package typestate

import boomerang.scene.ControlFlowGraph.Edge
import com.google.common.collect.{Lists, Sets}
import java.util.{Collection, Collections, HashSet, Set}
import typestate.finiteautomata.{ITransition, Transition}
import wpds.impl.Weight

class TransitionFunction(trans: Set[_ <: ITransition], var stateChangeStatements: Set[Edge]) extends Weight {

  private val value: Set[ITransition] = new HashSet(trans)
  private var rep: String = null

  def this(trans: ITransition, stateChangeStatements: Set[Edge]) {
    this(Set(trans), stateChangeStatements)
  };

  private def this(rep: String) {
    this(Set.empty[ITransition], Set.empty[Edge])
    this.rep = rep
    this.stateChangeStatements = Set.empty[Edge]
  };

  def values(): Collection[ITransition] = Lists.newArrayList(value);

  def getLastStateChangeStatements: Set[Edge] = stateChangeStatements;

  override def extendWith(other: Weight): Weight = {
    if (other == one()) return this
    if (this == one()) return other
    if (other == zero() || this == zero()) {
      return zero()
    }
    val func = other.asInstanceOf[TransitionFunction]
    val otherTransitions = func.value
    val ress = new HashSet[ITransition]()
    val newStateChangeStatements = new HashSet[Edge]()
    for (first <- value) {
      for (second <- otherTransitions) {
        if (second == Transition.identity()) {
          ress.add(first)
          newStateChangeStatements.addAll(stateChangeStatements)
        } else if (first == Transition.identity()) {
          ress.add(second)
          newStateChangeStatements.addAll(func.stateChangeStatements)
        } else if (first.to() == second.from()) {
          ress.add(new Transition(first.from(), second.to()))
          newStateChangeStatements.addAll(func.stateChangeStatements)
        }
      }
    }
    new TransitionFunction(ress, newStateChangeStatements)
  };

  override def combineWith(other: Weight): Weight = {
    if (!other.isInstanceOf[TransitionFunction]) throw new RuntimeException()
    if (this == zero()) return other
    if (other == zero()) return this
    if (other == one() && this == one()) {
      return one()
    }
    val func = other.asInstanceOf[TransitionFunction]
    if (other == one() || this == one()) {
      val transitions = new HashSet[ITransition](if (other == one()) value else func.value)
      val idTransitions = Sets.newHashSet[ITransition]()
      for (t <- transitions) {
        idTransitions.add(new Transition(t.from(), t.from()))
      }
      transitions.addAll(idTransitions)
      return new TransitionFunction(
        transitions,
        Sets.newHashSet(if (other == one()) stateChangeStatements else func.stateChangeStatements))
    }
    val transitions = new HashSet[ITransition](func.value)
    transitions.addAll(value)
    val newStateChangeStmts = Sets.newHashSet(stateChangeStatements)
    newStateChangeStmts.addAll(func.stateChangeStatements)
    new TransitionFunction(transitions, newStateChangeStmts)
  };

  override def toString: String = {
    if (rep != null) return rep
    "Weight: " + value.toString + ""
  };

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (rep == null) 0 else rep.hashCode)
    result = prime * result + (if (value == null) 0 else value.hashCode)
    result
  };

  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null) return false
    if (getClass != obj.getClass) return false
    val other = obj.asInstanceOf[TransitionFunction]
    if (rep == null) {
      if (other.rep != null) return false
    } else if (rep != other.rep) return false
    if (value == null) {
      if (other.value != null) return false
    } else if (value != other.value) return false
    true
  }
}

object TransitionFunction {
  private var one: TransitionFunction = _
  private var zero: TransitionFunction = _

  def one(): TransitionFunction = {
    if (one == null) one = new TransitionFunction("ONE")
    one
  }

  def zero(): TransitionFunction = {
    if (zero == null) zero = new TransitionFunction("ZERO")
    zero
  }
}
