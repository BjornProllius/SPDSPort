package wpds.impl

import wpds.interfaces.{Location, State}

class UPopRule[N <: Location, D <: State](s1: D, l1: N, s2: D) extends PopRule[N, D, Weight.NoWeight](s1, l1, s2, Weight.NO_WEIGHT_ONE) {

    override def toString: String = {
        "<" + s1 + ";" + l1 + ">-><" + s2 + ">"
    }
}