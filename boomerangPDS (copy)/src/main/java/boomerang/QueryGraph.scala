package boomerang

import boomerang.callgraph.CallerListener
import boomerang.callgraph.ObservableICFG
import boomerang.scene.ControlFlowGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.BackwardBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import boomerang.util.DefaultValueMap
import com.google.common.base.Joiner
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import java.util
import java.util.Map.Entry
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener

object QueryGraph {
  private val LOGGER = LoggerFactory.getLogger(classOf[QueryGraph[_ <: Nothing]])

  private trait AddTargetEdgeListener {
    def getTarget: Nothing

    def edgeAdded(queryEdge: QueryGraph.QueryEdge): Unit

    def noParentEdge(): Unit
  }

  private class QueryEdge(private val source: Nothing, private var node: Nothing, private val target: Nothing) {
    def getNode: Nothing = node

    def getSource: Nothing = source

    def getTarget: Nothing = target

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (source == null) 0
      else source.hashCode)
      result = prime * result + (if (node == null) 0
      else node.hashCode)
      result = prime * result + (if (target == null) 0
      else target.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[QueryGraph.QueryEdge]
      if (source == null) if (other.source != null) return false
      else if (!source.equals(other.source)) return false
      if (target == null) if (other.target != null) return false
      else if (!target.equals(other.target)) return false
      if (node == null) if (other.node != null) return false
      else if (!node.equals(other.node)) return false
      true
    }
  }
}

class QueryGraph[W <: Weight](weightedBoomerang: Nothing) {
  this.forwardSolvers = weightedBoomerang.getSolvers
  this.backwardSolver = weightedBoomerang.getBackwardSolvers
  this.icfg = weightedBoomerang.icfg
  final private var icfg: Nothing = null
  private val sourceToQueryEdgeLookUp = HashMultimap.create
  private val targetToQueryEdgeLookUp = HashMultimap.create
  private val roots = Sets.newHashSet
  private var forwardSolvers: Nothing = null
  private val edgeAddListener = HashMultimap.create
  private var backwardSolver: Nothing = null

  def addRoot(root: Nothing): Unit = {
    this.roots.add(root)
  }

  def addEdge(parent: Nothing, node: Nothing, child: Nothing): Unit = {
    val queryEdge = new QueryGraph.QueryEdge(parent, node, child)
    sourceToQueryEdgeLookUp.put(parent, queryEdge)
    if (targetToQueryEdgeLookUp.put(child, queryEdge)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(edgeAddListener.get(child))) {
        l.edgeAdded(queryEdge)
      }
    }
    getSolver(parent).getCallAutomaton.registerListener(new QueryGraph[W]#SourceListener(new Nothing(node.fact), parent, child, null))
  }

  private def getSolver(query: Nothing): Nothing = {
    if (query.isInstanceOf[Nothing]) return backwardSolver.get(query)
    forwardSolvers.get(query)
  }

  def unregisterAllListeners(): Unit = {
    this.edgeAddListener.clear
  }

  def getNodes: Nothing = {
    val nodes = Sets.newHashSet(sourceToQueryEdgeLookUp.keySet)
    nodes.addAll(targetToQueryEdgeLookUp.keySet)
    nodes
  }

  private class SourceListener(state: Nothing, private var parent: Nothing, private var child: Nothing, private var callee: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getStart.isInstanceOf[Nothing] && callee != null) {
        val callSiteLabel = t.getLabel
        getSolver(child).allowUnbalanced(callee, if (parent.isInstanceOf[Nothing]) callSiteLabel.getTarget
        else callSiteLabel.getStart)
      }
      if (t.getTarget.isInstanceOf[Nothing]) getSolver(parent).getCallAutomaton.registerListener(new QueryGraph[W]#SourceListener(t.getTarget, parent, child, t.getLabel.getMethod))
      if (weightedPAutomaton.isUnbalancedState(t.getTarget)) registerEdgeListener(new QueryGraph[W]#UnbalancedContextListener(child, parent, t))
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (callee == null) 0
      else callee.hashCode)
      result = prime * result + (if (child == null) 0
      else child.hashCode)
      result = prime * result + (if (parent == null) 0
      else parent.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[QueryGraph[W]#SourceListener]
      if (!getEnclosingInstance.equals(other.getEnclosingInstance)) return false
      if (callee == null) if (other.callee != null) return false
      else if (!callee.equals(other.callee)) return false
      if (child == null) if (other.child != null) return false
      else if (!child.equals(other.child)) return false
      if (parent == null) if (other.parent != null) return false
      else if (!parent.equals(other.parent)) return false
      true
    }

    private def getEnclosingInstance = thisQueryGraph
  }

  def toString: Nothing = {
    var s = ""
    var level = 0
    import scala.collection.JavaConversions._
    for (root <- roots) {
      s += "Root:" + root + "\n"
      s += visit(root, "", {
        level += 1; level
      }, Sets.newHashSet)
    }
    s
  }

  def registerEdgeListener(l: QueryGraph.AddTargetEdgeListener): Unit = {
    if (edgeAddListener.put(l.getTarget, l)) {
      val edges = Lists.newArrayList(targetToQueryEdgeLookUp.get(l.getTarget))
      import scala.collection.JavaConversions._
      for (edge <- edges) {
        l.edgeAdded(edge)
      }
      if (edges.isEmpty) l.noParentEdge()
    }
  }

  private class UnbalancedContextListener(private var child: Nothing, private var parent: Nothing, private val transition: Nothing) extends QueryGraph.AddTargetEdgeListener {
    @Override override def getTarget: Nothing = parent

    @Override override def edgeAdded(parentOfParent: QueryGraph.QueryEdge): Unit = {
      val newParent = parentOfParent.getSource
      getSolver(newParent).getCallAutomaton.registerListener(new QueryGraph[W]#SourceListener(new Nothing(parentOfParent.getNode.fact), newParent, child, null))
    }

    @Override override def noParentEdge(): Unit = {
      if (child.isInstanceOf[Nothing]) {
        val callee = transition.getTarget.fact.m
        icfg.addCallerListener(new Nothing() {
          @Override def getObservedCallee: Nothing = callee

          @Override def onCallerAdded(callSite: Nothing, method: Nothing): Unit = {
            getSolver(child).allowUnbalanced(callee, callSite)
          }
        })
      }
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getEnclosingInstance.hashCode
      result = prime * result + (if (child == null) 0
      else child.hashCode)
      result = prime * result + (if (parent == null) 0
      else parent.hashCode)
      result = prime * result + (if (transition == null) 0
      else transition.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[QueryGraph[W]#UnbalancedContextListener]
      if (!getEnclosingInstance.equals(other.getEnclosingInstance)) return false
      if (child == null) if (other.child != null) return false
      else if (!child.equals(other.child)) return false
      if (parent == null) if (other.parent != null) return false
      else if (!parent.equals(other.parent)) return false
      if (parent == null) if (other.transition != null) return false
      else if (!transition.equals(other.transition)) return false
      true
    }

    private def getEnclosingInstance = thisQueryGraph
  }

  private def visit(parent: Nothing, s: Nothing, i: Int, visited: Nothing) = {
    import scala.collection.JavaConversions._
    for (child <- sourceToQueryEdgeLookUp.get(parent)) {
      if (visited.add(child.getTarget)) {
        for (j <- 0 to i) {
          s += " "
        }
        s += i
        s += child + "\n"
        s += visit(child.getTarget, "", {
          i += 1; i
        }, visited)
      }
      else {
      }
    }
    s
  }

  def toDotString: Nothing = {
    var s = "digraph {\n"
    val trans = new Nothing
    import scala.collection.JavaConversions._
    for (target <- sourceToQueryEdgeLookUp.entries) {
      var v = "\t\"" + escapeQuotes(target.getKey.toString) + "\""
      v += " -> \"" + escapeQuotes(target.getValue.getTarget.toString) + "\""
      trans.add(v)
    }
    s += Joiner.on("\n").join(trans)
    s += "}\n"
    s
  }

  private def escapeQuotes(string: Nothing) = string.replace("\"", "")

  def isRoot(q: Nothing): Boolean = roots.contains(q)
}