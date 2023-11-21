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
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import java.util.Collections
import typestate.finiteautomata.ITransition
import typestate.finiteautomata.Transition
import wpds.impl.Weight

object TransitionFunction {
  private var one: TransitionFunction = null
  private var zero: TransitionFunction = null

  def one: TransitionFunction = {
    if (one == null) one = new TransitionFunction("ONE")
    one
  }

  def zero: TransitionFunction = {
    if (zero == null) zero = new TransitionFunction("ZERO")
    zero
  }
}

class TransitionFunction extends Nothing {
  final private var value: Nothing = null
  final private var rep: Nothing = null
  private var stateChangeStatements: Nothing = null

  def this(trans: Nothing, stateChangeStatements: Nothing) {
    this()
    this.stateChangeStatements = stateChangeStatements
    this.value = new Nothing(trans)
    this.rep = null
  }

  def this(trans: Nothing, stateChangeStatements: Nothing) {
    this(new Nothing(Collections.singleton(trans)), stateChangeStatements)
  }

  def this(rep: Nothing) {
    this()
    this.value = Sets.newHashSet
    this.rep = rep
    this.stateChangeStatements = Sets.newHashSet
  }

  def values: Nothing = Lists.newArrayList(value)

  def getLastStateChangeStatements: Nothing = stateChangeStatements

  @Override def extendWith(other: Nothing): Nothing = {
    if (other.equals(TransitionFunction.one)) return this
    if (this == TransitionFunction.one) return other
    if (other.equals(TransitionFunction.zero) || this == TransitionFunction.zero) return TransitionFunction.zero
    val func = other.asInstanceOf[TransitionFunction]
    val otherTransitions = func.value
    val ress = new Nothing
    val newStateChangeStatements = new Nothing
    import scala.collection.JavaConversions._
    for (first <- value) {
      import scala.collection.JavaConversions._
      for (second <- otherTransitions) {
        if (second.equals(Transition.identity)) {
          ress.add(first)
          newStateChangeStatements.addAll(stateChangeStatements)
        }
        else if (first.equals(Transition.identity)) {
          ress.add(second)
          newStateChangeStatements.addAll(func.stateChangeStatements)
        }
        else if (first.to.equals(second.from)) {
          ress.add(new Nothing(first.from, second.to))
          newStateChangeStatements.addAll(func.stateChangeStatements)
        }
      }
    }
    new TransitionFunction(ress, newStateChangeStatements)
  }

  @Override def combineWith(other: Nothing): Nothing = {
    if (!other.isInstanceOf[TransitionFunction]) throw new Nothing
    if (this == TransitionFunction.zero) return other
    if (other.equals(TransitionFunction.zero)) return this
    if (other.equals(TransitionFunction.one) && this == TransitionFunction.one) return TransitionFunction.one
    val func = other.asInstanceOf[TransitionFunction]
    if (other.equals(TransitionFunction.one) || this == TransitionFunction.one) {
      val transitions = new Nothing(if (other.equals(TransitionFunction.one)) value
      else func.value)
      val idTransitions = Sets.newHashSet
      import scala.collection.JavaConversions._
      for (t <- transitions) {
        idTransitions.add(new Nothing(t.from, t.from))
      }
      transitions.addAll(idTransitions)
      return new TransitionFunction(transitions, Sets.newHashSet(if (other.equals(TransitionFunction.one)) stateChangeStatements
      else func.stateChangeStatements))
    }
    val transitions = new Nothing(func.value)
    transitions.addAll(value)
    val newStateChangeStmts = Sets.newHashSet(stateChangeStatements)
    newStateChangeStmts.addAll(func.stateChangeStatements)
    new TransitionFunction(transitions, newStateChangeStmts)
  }

  def toString: Nothing = {
    if (this.rep != null) return this.rep
    "Weight: " + value.toString + ""
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result = prime * result + (if (value == null) 0
    else value.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[TransitionFunction]
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    if (value == null) if (other.value != null) return false
    else if (!value.equals(other.value)) return false
    true
  }
}