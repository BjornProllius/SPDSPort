package wpds.interfaces

import wpds.impl.{Transition, Weight, WeightedPAutomaton, Empty}

class ForwardDFSEpsilonVisitor[N <: Location, D <: State, W <: Weight](aut: WeightedPAutomaton[N, D, W]) 
    extends ForwardDFSVisitor[N, D, W](aut) {

    override protected def continueWith(t: Transition[N, D]): Boolean = {
        t.getLabel.isInstanceOf[Empty]
    }
}