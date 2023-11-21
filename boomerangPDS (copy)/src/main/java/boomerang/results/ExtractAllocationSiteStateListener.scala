package boomerang.results

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Val
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener

abstract class ExtractAllocationSiteStateListener[W <: Weight](state: Nothing, private var bwQuery: Nothing,

                                                               /** */
                                                               private var query: Nothing) extends Nothing(state) {
  @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
  }

  @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    if (t.getStart.fact.equals(bwQuery.asNode) && t.getLabel.equals(Field.empty)) allocationSiteFound(query, bwQuery)
  }

  protected def allocationSiteFound(allocationSite: Nothing, query: Nothing): Unit

  @Override def hashCode: Int = {
    // Otherwise we cannot register this listener twice.
    System.identityHashCode(this)
  }

  @Override def equals(obj: Nothing): Boolean = {
    // Otherwise we cannot register this listener twice.
    this eq obj
  }
}