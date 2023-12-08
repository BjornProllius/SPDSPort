package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State

trait ConnectPushListener[N <: Location, D <: State, W <: Weight] {
    def connect(returnSite: N, returnedFact: D, returnedWeight: W): Unit
}