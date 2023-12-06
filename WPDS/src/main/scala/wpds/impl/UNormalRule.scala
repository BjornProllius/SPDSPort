package wpds.impl

import wpds.interfaces.{Location, State}

class UNormalRule[N <: Location, D <: State] extends NormalRule[N, D, Weight.NoWeight](s1: D, l1: N, s2: D, l2: N, Weight.NO_WEIGHT_ONE) {

    override def toString: String = {
        "<" + s1 + ";" + l1 + ">-><" + s2 + ";" + l2 + ">"
    }
}