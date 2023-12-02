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
package typestate

import boomerang.scene.ControlFlowGraph.Edge
import com.google.common.collect.{Lists, Sets}
import java.util.{Collection, Collections, HashSet, Set}
import typestate.finiteautomata.{ITransition, Transition}
import wpds.impl.Weight

class TransitionFunction(trans: Set[_ <: ITransition], stateChangeStatements: Set[Edge])
  extends Weight {

  private val value: Set[ITransition] = new HashSet[ITransition](trans)
  private val rep: String = null
  private var stateChangeStatementsVar: Set[Edge] = stateChangeStatements

  def this(trans: ITransition, stateChangeStatements: Set[Edge]) {
    this(new HashSet[ITransition](Collections.singleton(trans)), stateChangeStatements)
  }

  private def this(rep: String) {
    this(Sets.newHashSet[ITransition](), Sets.newHashSet[Edge]())
    this.rep = rep
  }

  def values(): Collection[ITransition] = Lists.newArrayList[ITransition](value)

  def getLastStateChangeStatements: Set[Edge] = stateChangeStatementsVar

  override def extendWith(other: Weight): Weight = {
    if (other == one) return this
    if (this == one) return other
    if (other == zero || this == zero) {
      return zero
    }
    val func: TransitionFunction = other.asInstanceOf[TransitionFunction]
    val otherTransitions: Set[ITransition] = func.value
    val ress: Set[ITransition] = new HashSet[ITransition]()
    val newStateChangeStatements: Set[Edge] = new HashSet[Edge]()
    for (first <- value) {
      for (second <- otherTransitions) {
        if (second == Transition.identity) {
          ress.add(first)
          newStateChangeStatements.addAll(stateChangeStatementsVar)
        } else if (first == Transition.identity) {
          ress.add(second)
          newStateChangeStatements.addAll(func.stateChangeStatementsVar)
        } else if (first.to == second.from) {
          ress.add(new Transition(first.from, second.to))
          newStateChangeStatements.addAll(func.stateChangeStatementsVar)
        }
      }
    }
    new TransitionFunction(ress, newStateChangeStatements)
  }

  override def combineWith(other: Weight): Weight = {
    if (!other.isInstanceOf[TransitionFunction]) throw new RuntimeException()
    if (this == zero) return other
    if (other == zero) return this
    if (other == one && this == one) {
      return one
    }
    val func: TransitionFunction = other.asInstanceOf[TransitionFunction]
    if (other == one || this == one) {
      val transitions: Set[ITransition] = new HashSet[ITransition](if (other == one) value else func.value)
      val idTransitions: Set[ITransition] = Sets.newHashSet[ITransition]()
      for (t <- transitions) {
        idTransitions.add(new Transition(t.from, t.from))
      }
      transitions.addAll(idTransitions)
      return new TransitionFunction(
        transitions,
        Sets.newHashSet(if (other == one) stateChangeStatementsVar else func.stateChangeStatementsVar)
      )
    }
    val transitions: Set[ITransition] = new HashSet[ITransition](func.value)
    transitions.addAll(value)
    val newStateChangeStmts: HashSet[Edge] = Sets.newHashSet(stateChangeStatementsVar)
    newStateChangeStmts.addAll(func.stateChangeStatementsVar)
    new TransitionFunction(transitions, newStateChangeStmts)
  }

  override def toString: String = {
    if (rep != null) return rep
    s"Weight: $value"
  }

  override def hashCode(): Int = {
    val prime: Int = 31
    var result: Int = 1
    result = prime * result + (if (rep == null) 0 else rep.hashCode)
    result = prime * result + (if (value == null) 0 else value.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (this eq obj.asInstanceOf[AnyRef]) return true
    if (obj == null) return false
    if (getClass != obj.getClass) return false
    val other: TransitionFunction = obj.asInstanceOf[TransitionFunction]
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
