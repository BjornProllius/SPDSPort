package ideal

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import com.google.common.collect.{HashMultimap, Lists, Multimap, Sets}
import ideal.IDEALSeedSolver.Phases

import scala.collection.JavaConverters._

class IDEALWeightFunctions[W <: Weight](
    private val delegate: WeightFunctions[Edge, Val, Edge, W],
    private val strongUpdates: Boolean
) extends WeightFunctions[Edge, Val, Edge, W] {

  private var listeners: Set[NonOneFlowListener] = Sets.newHashSet()
  private var potentialStrongUpdates: Set[Edge] = Sets.newHashSet()
  private var weakUpdates: Set[Edge] = Sets.newHashSet()
  private var nonOneFlowNodes: Set[Node[Edge, Val]] = Sets.newHashSet()
  private var phase: Phases = _
  private var indirectAlias: Multimap[Node[Edge, Val], Node[Edge, Val]] = HashMultimap.create()
  private var nodesWithStrongUpdate: Set[Node[Edge, Val]] = Sets.newHashSet()

  def push(curr: Node[Edge, Val], succ: Node[Edge, Val], calleeSp: Edge): W = {
    val weight = delegate.push(curr, succ, calleeSp)
    if (isObjectFlowPhase && !weight.equals(getOne)) {
      if (succ.isInstanceOf[PushNode[Edge, Val, Edge]]) {
        val pushNode = succ.asInstanceOf[PushNode[Edge, Val, Edge]]
        addOtherThanOneWeight(new Node(pushNode.location(), curr.fact()))
      }
    }
    weight
  }

  def addOtherThanOneWeight(curr: Node[Edge, Val]): Unit = {
    if (nonOneFlowNodes.add(curr)) {
      listeners.asScala.foreach(_.nonOneFlow(curr))
    }
  }

  def normal(curr: Node[Edge, Val], succ: Node[Edge, Val]): W = {
    val weight = delegate.normal(curr, succ)
    if (isObjectFlowPhase && succ.stmt.getTarget.containsInvokeExpr && !weight.equals(getOne)) {
      addOtherThanOneWeight(succ)
    }
    weight
  }

  def pop(curr: Node[Edge, Val]): W = {
    delegate.pop(curr)
  }

  def registerListener(listener: NonOneFlowListener): Unit = {
    if (listeners.add(listener)) {
      nonOneFlowNodes.asScala.foreach(listener.nonOneFlow)
    }
  }

  def getOne: W = {
    delegate.getOne
  }

  override def toString: String = {
    s"[IDEAL-Wrapped Weights] ${delegate.toString}"
  }

  def potentialStrongUpdate(stmt: Edge): Unit = {
    potentialStrongUpdates.add(stmt)
  }

  def weakUpdate(stmt: Edge): Unit = {
    weakUpdates.add(stmt)
  }

  def setPhase(phase: Phases): Unit = {
    this.phase = phase
  }

  def addIndirectFlow(source: Node[Edge, Val], target: Node[Edge, Val]): Unit = {
    if (source == target) return
    println(s"Alias flow detected $source $target")
    indirectAlias.put(source, target)
  }
 
  def getAliasesFor(node: Node[Edge, Val]): Collection[Node[Edge, Val]] = {
    indirectAlias.get(node).asScala
  }

  def isStrongUpdateStatement(stmt: Edge): Boolean = {
    potentialStrongUpdates.contains(stmt) && !weakUpdates.contains(stmt) && strongUpdates
  }

  def isKillFlow(node: Node[Edge, Val]): Boolean = {
    !nodesWithStrongUpdate.contains(node)
  }

  def addNonKillFlow(curr: Node[Edge, Val]): Unit = {
    nodesWithStrongUpdate.add(curr)
  }

  private def isObjectFlowPhase: Boolean = {
    phase == Phases.ObjectFlow
  }
}
