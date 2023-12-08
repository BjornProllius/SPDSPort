package wpds.impl

import scala.collection.mutable.Set
import wpds.interfaces.{Location, State, WPAStateListener}


abstract class StackListener[N <: Location, D <: State, W <: Weight](val aut: WeightedPAutomaton[N, D, W], state: D, var source: N) 
    extends WPAStateListener[N, D, W](state) {

    private val notifiedStacks: Set[N] = Set()

    override def onOutTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
        if (t.getLabel == aut.epsilon()) return
        if (this.aut.getInitialStates.contains(t.getTarget)) {
            if (t.getLabel == source) {
                anyContext(source)
            }
            return
        }
        if (this.aut.isGeneratedState(t.getTarget)) {
            aut.registerListener(new SubStackListener(t.getTarget, this))
        }
    }

    override def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {}

    def stackElement(callSite: N): Unit

    def anyContext(end: N): Unit

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + (if (source == null) 0 else source.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case other: StackListener[_, _, _] =>
            (this eq other) || (other != null && getClass == other.getClass &&
                source == other.source)
        case _ => false
    }


    private class SubStackListener(state: D, parent: StackListener) extends WPAStateListener[N, D, W](state) {

        override def onOutTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
            if (t.getLabel == aut.epsilon()) return
            stackElement(t.getLabel)
            if (aut.isGeneratedState(t.getTarget) && t.getTarget != t.getStart) {
                aut.registerListener(new SubStackListener(t.getTarget, parent))
            }
        }

        override def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {}

        def stackElement(parent: N): Unit = {
            if (notifiedStacks.add(parent)) {
                StackListener.this.stackElement(parent)
            }
        }

        override def hashCode(): Int = {
            val prime = 31
            var result = super.hashCode()
            result = prime * result + (if (parent == null) 0 else parent.hashCode())
            result
        }

        override def equals(obj: Any): Boolean = obj match {
            case other: SubStackListener =>
                (this eq other) || (other != null && getClass == other.getClass &&
                    parent == other.parent)
            case _ => false
        }
    }
}