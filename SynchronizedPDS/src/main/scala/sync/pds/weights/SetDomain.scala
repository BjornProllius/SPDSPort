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
package sync.pds.weights

import com.google.common.collect.Sets
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight
import wpds.interfaces.Location

object SetDomain {
  private var one: SetDomain[_, _, _] = null
  private var zero: SetDomain[_, _, _] = null

  def one[N <: Location, Stmt, Fact]: SetDomain[N, Stmt, Fact] = {
    if (one == null) one = new SetDomain[_, _, _]("<1>")
    one
  }

  def zero[N <: Location, Stmt, Fact]: SetDomain[N, Stmt, Fact] = {
    if (zero == null) zero = new SetDomain[_, _, _]("<0>")
    zero
  }
}

class SetDomain[N, Stmt, Fact] extends Nothing {
  final private var rep: Nothing = null
  private var nodes: Nothing = null

  def this(rep: Nothing) {
    this()
    this.rep = rep
  }

  def this(nodes: Nothing) {
    this()
    this.nodes = nodes
    this.rep = null
  }

  def this(node: Nothing) {
    this()
    this.nodes = Sets.newHashSet[Nothing](node)
    this.rep = null
  }

  @Override def extendWith(other: Nothing): Nothing = {
    if (other.equals(SetDomain.one)) return this
    if (this == SetDomain.one) return other
    SetDomain.zero
  }

  @Override def combineWith(other: Nothing): Nothing = {
    if (other.equals(SetDomain.zero)) return this
    if (this == SetDomain.zero) return other
    if (this == SetDomain.one || other.equals(SetDomain.one)) return SetDomain.one
    if (other.isInstanceOf[SetDomain[_, _, _]]) {
      val merged = Sets.newHashSet(nodes)
      merged.addAll(other.asInstanceOf[SetDomain[_, _, _]].nodes)
      return new SetDomain[N, Stmt, Fact](merged)
    }
    SetDomain.zero
  }

  @Override def toString: Nothing = {
    if (rep != null) return rep
    nodes.toString
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (nodes == null) 0
    else nodes.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[SetDomain[_, _, _]]
    if (nodes == null) if (other.nodes != null) return false
    else if (!nodes.equals(other.nodes)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    true
  }

  def elements: Nothing = Sets.newHashSet(nodes)
}