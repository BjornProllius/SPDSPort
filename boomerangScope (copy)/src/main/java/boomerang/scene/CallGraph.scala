package boomerang.scene

import com.google.common.collect.HashMultimap
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util

object CallGraph {
  class Edge(private val callSite: Nothing, private val callee: Nothing) {
    assert(callSite.containsInvokeExpr)

    def tgt: Nothing = callee

    def src: Nothing = callSite

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (callSite == null) 0
      else callSite.hashCode)
      result = prime * result + (if (callee == null) 0
      else callee.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[CallGraph.Edge]
      if (callSite == null) if (other.callSite != null) return false
      else if (!callSite.equals(other.callSite)) return false
      if (callee == null) if (other.callee != null) return false
      else if (!callee.equals(other.callee)) return false
      true
    }

    @Override def toString: Nothing = "Call Graph Edge: " + callSite + " calls " + tgt
  }
}

class CallGraph {
  private val edges = Sets.newHashSet
  private val edgesOutOf = HashMultimap.create
  private val edgesInto = HashMultimap.create
  private val entryPoints = Sets.newHashSet
  private val fieldLoadStatements = HashMultimap.create
  private val fieldStoreStatements = HashMultimap.create

  def edgesOutOf(stmt: Nothing): Nothing = edgesOutOf.get(stmt)

  def addEdge(edge: CallGraph.Edge): Boolean = {
    edgesOutOf.put(edge.callSite, edge)
    edgesInto.put(edge.tgt, edge)
    computeStaticFieldsLoadAndStores(edge.tgt)
    edges.add(edge)
  }

  def edgesInto(m: Nothing): Nothing = edgesInto.get(m)

  def size: Int = edges.size

  def getEdges: Nothing = edges

  def getEntryPoints: Nothing = entryPoints

  def addEntryPoint(m: Nothing): Boolean = {
    computeStaticFieldsLoadAndStores(m)
    entryPoints.add(m)
  }

  def getReachableMethods: Nothing = {
    val reachableMethod = Sets.newHashSet
    reachableMethod.addAll(entryPoints)
    reachableMethod.addAll(edgesInto.keySet)
    reachableMethod
  }

  def getFieldStoreStatements: Nothing = fieldStoreStatements

  def getFieldLoadStatements: Nothing = fieldLoadStatements

  private def computeStaticFieldsLoadAndStores(m: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (s <- m.getStatements) {
      if (s.isStaticFieldStore) fieldStoreStatements.put(s.getStaticField.field, s)
      if (s.isStaticFieldLoad) fieldLoadStatements.put(s.getStaticField.field, s)
    }
  }
}