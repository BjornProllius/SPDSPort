package wpds.interfaces

import wpds.impl.{Transition, Weight, WeightedPAutomaton}

abstract class WPAStateListener[N <: Location, D <: State, W <: Weight](val state: D) {

    def onOutTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit

    def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit

    def getState: D = state

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + (if (state == null) 0 else state.hashCode)
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: WPAStateListener[N, D, W] => state == that.state
        case _ => false
    }
}