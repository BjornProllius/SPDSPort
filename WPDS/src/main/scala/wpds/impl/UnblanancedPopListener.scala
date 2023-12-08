package wpds.impl

import wpds.interfaces.{Location, State}

trait UnbalancedPopListener[N <: Location, D <: State, W <: Weight] {
    def unbalancedPop(returningFact: D, trans: Transition[N, D], weight: W): Unit
}