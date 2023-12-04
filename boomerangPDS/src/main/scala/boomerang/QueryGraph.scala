package boomerang

import boomerang.callgraph.{CallerListener,ObservableICFG}import boomerang.scene.{ControlFlowGraph,Method,Statement,Val}import boomerang.solver.{AbstractBoomerangSolver,BackwardBoomerangSolver,ForwardBoomerangSolver}

import boomerang.util.DefaultValueMap import com.google.common.collect.{HashMultimap,Lists,Multimap,Sets}
import com.google.common.base.Joiner import java.util.{ArrayList,Map,Entry,Set,TreeSet}import org.slf4j.{Logger,LoggerFactory}import sync.pds.solver.nodes.{GeneratedState,INode,Node,SingleNode}import wpds.impl.{Transition,Weight,WeightedPAutomaton}
import wpds.interfaces.WPAStateListener

class QueryGraph[W<:Weight](weightedBoomerang:WeightedBoomerang[W])
{
  private val LOGGER:Logger=LoggerFactory.getLogger(classOf[QueryGraph[_]])
  private val icfg:ObservableICFG[Statement,Method]=weightedBoomerang.icfg
  private var sourceToQueryEdgeLookUp:Multimap[Query,QueryEdge]=HashMultimap.create()
  private var targetToQueryEdgeLookUp:Multimap[Query,QueryEdge]=HashMultimap.create()
  private var roots:Set[Query]=Sets.newHashSet()
  private val forwardSolvers:DefaultValueMap[ForwardQuery,ForwardBoomerangSolver[W]]=weightedBoomerang.getSolvers
  private var edgeAddListener:Multimap[Query,AddTargetEdgeListener]=HashMultimap.create()
  private val backwardSolver:DefaultValueMap[BackwardQuery,BackwardBoomerangSolver[W]]=
  weightedBoomerang.getBackwardSolvers

  def

  addRoot(root: Query): Unit = {
    this.roots.add(root)
  }

  def addEdge(parent: Query, node: Node[Edge, Val], child: Query): Unit = {
    val queryEdge = new QueryEdge(parent, node, child)
    sourceToQueryEdgeLookUp.put(parent, queryEdge)
    if (targetToQueryEdgeLookUp.put(child, queryEdge)) {
      for (l <- Lists.newArrayList(edgeAddListener.get(child))) {
        l.edgeAdded(queryEdge)
      }
    }
    getSolver(parent)
      .getCallAutomaton
      .registerListener(new SourceListener(new SingleNode(node.fact), parent, child, null))
  }

  private def getSolver(query: Query): AbstractBoomerangSolver[W] = {
    query match {
      case _: BackwardQuery => backwardSolver.get(query)
      case _ => forwardSolvers.get(query)
    }
  }

  def unregisterAllListeners(): Unit = {
    this.edgeAddListener.clear()
  }

  def getNodes: Set[Query] = {
    val nodes = Sets.newHashSet(sourceToQueryEdgeLookUp.keySet())
    nodes.addAll(targetToQueryEdgeLookUp.keySet())
    nodes
  }

  private class SourceListener(state: INode[Val], parent: Query, child: Query, callee: Method) extends WPAStateListener[Edge, INode[Val], W](state) {

    override def onOutTransitionAdded(t: Transition[Edge, INode[Val]], w: W, weightedPAutomaton: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
      if (t.getStart.isInstanceOf[GeneratedState] && callee != null) {
        val callSiteLabel = t.getLabel
        getSolver(child).allowUnbalanced(callee, if (parent.isInstanceOf[BackwardQuery]) callSiteLabel.getTarget else callSiteLabel.getStart)
      }
      if (t.getTarget.isInstanceOf[GeneratedState]) {
        getSolver(parent).getCallAutomaton.registerListener(new SourceListener(t.getTarget, parent, child, t.getLabel.getMethod))
      }
      if (weightedPAutomaton.isUnbalancedState(t.getTarget)) {
        registerEdgeListener(new UnbalancedContextListener(child, parent, t))
      }
    }

    override def onInTransitionAdded(t: Transition[Edge, INode[Val]], w: W, weightedPAutomaton: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {}

    override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + QueryGraph.this.hashCode
      result = prime * result + (if (callee == null) 0 else callee.hashCode)
      result = prime * result + (if (child == null) 0 else child.hashCode)
      result = prime * result + (if (parent == null) 0 else parent.hashCode)
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: SourceListener =>
          if (this == other) return true
          if (!super.equals(other)) return false
          if (callee == null) {
            if (other.callee != null) return false
          } else if (!callee.equals(other.callee)) return false
          if (child == null) {
            if (other.child != null) return false
          } else if (!child.equals(other.child)) return false
          if (parent == null) {
            if (other.parent != null) return false
          } else if (!parent.equals(other.parent)) return false
          true
        case _ => false
      }
    }
  }

  override def toString: String = {
    var s = ""
    var level = 0
    for (root <- roots) {
      s += "Root:" + root + "\n"
      s += visit(root, "", level + 1, Sets.newHashSet())
    }
    s
  }

  def registerEdgeListener(l: AddTargetEdgeListener): Unit = {
    if (edgeAddListener.put(l.getTarget, l)) {
      val edges = Lists.newArrayList(targetToQueryEdgeLookUp.get(l.getTarget))
      for (edge <- edges) {
        l.edgeAdded(edge)
      }
      if (edges.isEmpty) {
        l.noParentEdge()
      }
    }
  }

  private trait AddTargetEdgeListener {
    def getTarget: Query

    def edgeAdded(queryEdge: QueryEdge): Unit

    def noParentEdge(): Unit
  }

  private class UnbalancedContextListener(child: Query, var parent: Query, transition: Transition[Edge, INode[Val]]) extends AddTargetEdgeListener {

    override def getTarget: Query = parent

    override def edgeAdded(parentOfParent: QueryEdge): Unit = {
      val newParent = parentOfParent.getSource
      getSolver(newParent)
        .getCallAutomaton
        .registerListener(new SourceListener(new SingleNode(parentOfParent.getNode.fact), newParent, child, null))
    }

    override def noParentEdge(): Unit = {
      if (child.isInstanceOf[BackwardQuery]) {
        val callee = transition.getTarget.fact.m()
        icfg.addCallerListener(new CallerListener[Statement, Method] {
          override def getObservedCallee: Method = callee

          override def onCallerAdded(callSite: Statement, method: Method): Unit = {
            getSolver(child).allowUnbalanced(callee, callSite)
          }
        })
      }
    }

    override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + QueryGraph.this.hashCode
      result = prime * result + (if (child == null) 0 else child.hashCode)
      result = prime * result + (if (parent == null) 0 else parent.hashCode)
      result = prime * result + (if (transition == null) 0 else transition.hashCode)
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: UnbalancedContextListener =>
          if (this == other) return true
          if (child == null) {
            if (other.child != null) return false
          } else if (!child.equals(other.child)) return false
          if (parent == null) {
            if (other.parent != null) return false
          } else if (!parent.equals(other.parent)) return false
          if (transition == null) {
            if (other.transition != null) return false
          } else if (!transition.equals(other.transition)) return false
          true
        case _ => false
      }
    }
  }

  private def visit(parent: Query, s: String, i: Int, visited: Set[Query]): String = {
    for (child <- sourceToQueryEdgeLookUp.get(parent)) {
      if (visited.add(child.getTarget)) {
        var s = s
        for (_ <- 0 to i) {
          s += " "
        }
        s += i
        s += child + "\n"
        s += visit(child.getTarget, "", i + 1, visited)
      }
    }
    s
  }

  def toDotString: String = {
    var s = "digraph {\n"
    val trans = new TreeSet[String]()
    for (target <- sourceToQueryEdgeLookUp.entries()) {
      var v = "\t\"" + escapeQuotes(target.getKey.toString) + "\""
      v += " -> \"" + escapeQuotes(target.getValue.getTarget.toString) + "\""
      trans.add(v)
    }
    s += Joiner.on("\n").join(trans)
    s += "}\n"
    s
  }

  private def escapeQuotes(string: String): String = {
    string.replace("\"", "")
  }

  private class QueryEdge(val source: Query, var node: Node[ControlFlowGraph.Edge, Val], val target: Query) {

    def getNode: Node[Edge, Val] = node

    def getSource: Query = source

    def getTarget: Query = target

    override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (source == null) 0 else source.hashCode)
      result = prime * result + (if (node == null) 0 else node.hashCode)
      result = prime * result + (if (target == null) 0 else target.hashCode)
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: QueryEdge =>
          if (this == other) return true
          if (source == null) {
            if (other.source != null) return false
          } else if (!source.equals(other.source)) return false
          if (target == null) {
            if (other.target != null) return false
          } else if (!target.equals(other.target)) return false
          if (node == null) {
            if (other.node != null) return false
          } else if (!node.equals(other.node)) return false
          true
        case _ => false
      }
    }
  }

  def isRoot(q: Query): Boolean = {
    roots.contains(q)
  }
}