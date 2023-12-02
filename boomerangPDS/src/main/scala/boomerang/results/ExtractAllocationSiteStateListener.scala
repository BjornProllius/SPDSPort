import boomerang.{BackwardQuery, ForwardQuery}
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.{Field, Val}
import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.WPAStateListener

abstract class ExtractAllocationSiteStateListener[W <: Weight](
    state: INode[Node[Edge, Val]],
    var bwQuery: BackwardQuery,
    var query: ForwardQuery
) extends WPAStateListener[Field, INode[Node[Edge, Val]], W](state) {

  // Otherwise we cannot register this listener twice.
  override def hashCode(): Int = System.identityHashCode(this)

  // Otherwise we cannot register this listener twice.
  override def equals(obj: Any): Boolean = this eq obj

  override def onOutTransitionAdded(
      t: Transition[Field, INode[Node[Edge, Val]]],
      w: W,
      weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
  ): Unit = {}

  override def onInTransitionAdded(
      t: Transition[Field, INode[Node[Edge, Val]]],
      w: W,
      weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
  ): Unit = {
    if (t.getStart().fact().equals(bwQuery.asNode()) && t.getLabel().equals(Field.empty())) {
      allocationSiteFound(query, bwQuery)
    }
  }

  protected def allocationSiteFound(allocationSite: ForwardQuery, query: BackwardQuery): Unit
}