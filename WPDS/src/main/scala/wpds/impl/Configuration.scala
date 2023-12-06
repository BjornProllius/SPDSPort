package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State

case class Configuration[N <: Location, D <: State](location: N, state: D)