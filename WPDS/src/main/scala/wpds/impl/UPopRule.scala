package wpds.impl

import wpds.interfaces.{Location, State}

class UPopRule[N <: Location, D <: State] extends PopRule[N, D, Weight.NoWeight](s1: D, l1: N, s2: D, Weight.NO_WEIGHT_ONE) {

    override def toString: String = {
        "<" + s1 + ";" + l1 + ">-><" + s2 + ">"
    }
}