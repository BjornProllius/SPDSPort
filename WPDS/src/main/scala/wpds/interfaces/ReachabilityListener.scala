package wpds.interfaces

import wpds.impl.Transition

trait ReachabilityListener[N <: Location, D <: State] {
    def reachable(t: Transition[N, D]): Unit
}