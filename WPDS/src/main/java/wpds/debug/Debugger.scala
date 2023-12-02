package wpds.debug

import wpds.impl.Weight
import wpds.interfaces.Location
import wpds.interfaces.State

trait Debugger[N <: Location, D <: State, W <: Weight]