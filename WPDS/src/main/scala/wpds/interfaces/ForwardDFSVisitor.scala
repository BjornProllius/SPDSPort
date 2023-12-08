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

import com.google.common.collect.{HashBasedTable, HashMultimap, Lists, Multimap, Table}
import java.util.LinkedList
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import scala.jdk.CollectionConverters._

class ForwardDFSVisitor[N <: Location, D <: State, W <: Weight](var aut: WeightedPAutomaton[N, D, W]) extends WPAUpdateListener[N, D, W] {
    private val listeners = HashMultimap.create[D, ReachabilityListener[N, D]]()
    private val adjacent = HashMultimap.create[D, D]()
    private val reaches = HashMultimap.create[D, D]()
    private val inverseReaches = HashMultimap.create[D, D]()
    private val refCount = HashBasedTable.create[D, D, Integer]()
  



    def registerListener(state: D, l: ReachabilityListener[N, D]): Unit = {
        if (listeners.put(state, l)) {
            Lists.newArrayList(inverseReaches.get(state)).asScala.foreach { d =>
                aut.registerListener(new TransitiveClosure(d, state, l))
            }
        }
    }


    private class TransitiveClosure(state: D, var s: D, var listener: ReachabilityListener[N, D]) extends WPAStateListener[N, D, W](state) {

        override def onOutTransitionAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
            listener.reachable(t)
        }

        override def onInTransitionAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {}

        override def hashCode(): Int = {
            val prime = 31
            var result = super.hashCode()
            result = prime * result + getOuterType().hashCode()
            result = prime * result + (if (listener == null) 0 else listener.hashCode())
            result = prime * result + (if (s == null) 0 else s.hashCode())
            result
        }

        override def equals(obj: Any): Boolean = {
            if (this == obj) return true
            if (!super.equals(obj)) return false
            if (getClass != obj.getClass) return false
            val other = obj.asInstanceOf[TransitiveClosure]
            if (!getOuterType().equals(other.getOuterType())) return false
            if (s == null) {
                if (other.s != null) return false
            } else if (!s.equals(other.s)) return false
            if (listener == null) {
                if (other.listener != null) return false
            } else if (!listener.equals(other.listener)) return false
            true
        }

        private def getOuterType(): ForwardDFSVisitor[N, D, W] = {
            ForwardDFSVisitor.this
        }
    }

    protected def continueWith(t: Transition[N, D]): Boolean = {
        true
    }

    override def onWeightAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {

        val a = t.getStart()
        val b = t.getTarget()
        inverseReaches(a, a)
        // inverseReaches(b,b);
        if (!continueWith(t)) return
        insertEdge(a, b)
    }

    private def insertEdge(a: D, b: D): Unit = {
        val worklist = new LinkedList[Edge]()
        if (refCount.get(a, b) == null) {
            makeClosure(a, b)
            worklist.add(new Edge(a, b))
            refCount.put(a, b, 1)
        }
        makeEdge(a, b)

        Lists.newArrayList(reaches.get(a)).asScala.foreach { x =>
            if (refCount.get(x, b) == null) {
                makeClosure(x, b)
                worklist.add(new Edge(x, b))
                refCount.put(x, b, 1)
            }
        }
        while (!worklist.isEmpty) {
            val e = worklist.poll()
            val x = e.from
            val y = e.to
            Lists.newArrayList(adjacent.get(y)).asScala.foreach { z =>
                if (refCount.get(x, z) == null) {
                    makeClosure(x, z)
                    worklist.add(new Edge(x, z))
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
            Lists.newArrayList(listeners.get(from)).asScala.foreach { l =>
                aut.registerListener(new TransitiveClosure(to, from, l))
            }
        }
    }

    private def makeClosure(from: D, to: D): Unit = {
        if (reaches.put(to, from)) {
            inverseReaches(from, to)
        }
    }

    private class Index {
        var refcount: Int = _
    }
    
    private class Edge(val from: D, val to: D) {
        override def hashCode(): Int = {
            val prime = 31
            var result = 1
            result = prime * result + (if (from == null) 0 else from.hashCode())
            result = prime * result + (if (to == null) 0 else to.hashCode())
            result
        }

        override def equals(obj: Any): Boolean = {
            if (this == obj) return true
            if (obj == null) return false
            if (getClass != obj.getClass) return false
            val other = obj.asInstanceOf[Edge]
            if (from == null) {
                if (other.from != null) return false
            } else if (!from.equals(other.from)) return false
            if (to == null) {
                if (other.to != null) return false
            } else if (!to.equals(other.to)) return false
            true
        }
    }

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + (if (aut == null) 0 else aut.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = {
        if (this == obj) return true
        if (!super.equals(obj)) return false
        if (getClass != obj.getClass) return false
        val other = obj.asInstanceOf[ForwardDFSVisitor[N, D, W]]
        if (aut == null) {
            if (other.aut != null) return false
        } else if (!aut.equals(other.aut)) return false
        true
    }
}