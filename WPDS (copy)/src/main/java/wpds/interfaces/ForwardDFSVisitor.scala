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
package wpds.interfaces

import com.google.common.collect.HashBasedTable
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Table
import java.util
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton

class ForwardDFSVisitor[N <: Location, D <: State, W <: Weight](protected var aut: Nothing) extends Nothing {
  private val listeners = HashMultimap.create
  private val adjacent = HashMultimap.create
  private val reaches = HashMultimap.create
  private val inverseReaches = HashMultimap.create
  private val refCount = HashBasedTable.create

  def registerListener(state: D, l: Nothing): Unit = {
    if (listeners.put(state, l)) {
      import scala.collection.JavaConversions._
      for (d <- Lists.newArrayList(inverseReaches.get(state))) {
        aut.registerListener(new ForwardDFSVisitor[N, D, W]#TransitiveClosure(d, state, l))
      }
    }
  }

  private class TransitiveClosure(state: D, private var s: D, private var listener: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      listener.reachable(t)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (listener == null) 0
      else listener.hashCode)
      result = prime * result + (if (s == null) 0
      else s.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ForwardDFSVisitor[N, D, W]#TransitiveClosure]
      if (!(getOuterType == other.getOuterType)) return false
      if (s == null) if (other.s != null) return false
      else if (!s.equals(other.s)) return false
      if (listener == null) if (other.listener != null) return false
      else if (!listener.equals(other.listener)) return false
      true
    }

    private def getOuterType = thisForwardDFSVisitor
  }

  protected def continueWith(t: Nothing) = true

  @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    val a = t.getStart
    val b = t.getTarget
    inverseReaches(a, a)
    // inverseReaches(b,b);
    if (!continueWith(t)) return
    insertEdge(a, b)
  }

  private def insertEdge(a: D, b: D): Unit = {
    val worklist = Lists.newLinkedList
    if (refCount.get(a, b) == null) {
      makeClosure(a, b)
      worklist.add(new ForwardDFSVisitor[N, D, W]#Edge(a, b))
      refCount.put(a, b, 1)
    }
    makeEdge(a, b)
    import scala.collection.JavaConversions._
    for (x <- Lists.newArrayList(reaches.get(a))) {
      if (refCount.get(x, b) == null) {
        makeClosure(x, b)
        worklist.add(new ForwardDFSVisitor[N, D, W]#Edge(x, b))
        refCount.put(x, b, 1)
      }
    }
    while (!worklist.isEmpty) {
      val e = worklist.poll
      val x = e.from
      val y = e.to
      import scala.collection.JavaConversions._
      for (z <- Lists.newArrayList(adjacent.get(y))) {
        if (refCount.get(x, z) == null) {
          makeClosure(x, z)
          worklist.add(new ForwardDFSVisitor[N, D, W]#Edge(x, z))
          refCount.put(x, z, 1)
        }
      }
    }
  }

  private def makeEdge(from: D, to: D): Unit = {
    adjacent.put(from, to)
    inverseReaches(from, to)
  }

  private def inverseReaches(from: D, to: D): Unit = {
    if (inverseReaches.put(from, to)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(listeners.get(from))) {
        aut.registerListener(new ForwardDFSVisitor[N, D, W]#TransitiveClosure(to, from, l))
      }
    }
  }

  private def makeClosure(from: D, to: D): Unit = {
    if (reaches.put(to, from)) inverseReaches(from, to)
  }

  private class Index {
    private[interfaces] val refcount = 0
  }

  private class Edge private(private[interfaces] val from: D, private[interfaces] val to: D) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (from == null) 0
      else from.hashCode)
      result = prime * result + (if (to == null) 0
      else to.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ForwardDFSVisitor[N, D, W]#Edge]
      if (from == null) if (other.from != null) return false
      else if (!from.equals(other.from)) return false
      if (to == null) if (other.to != null) return false
      else if (!to.equals(other.to)) return false
      true
    }
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (aut == null) 0
    else aut.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ForwardDFSVisitor[_ <: Nothing, _ <: Nothing, _ <: Nothing]]
    if (aut == null) if (other.aut != null) return false
    else if (!aut.equals(other.aut)) return false
    true
  }
}