package wpds.impl

import wpds.interfaces.{Location, State}
import wpds.impl.Weight

class UPushRule[N <: Location, D <: State](s1: D, l1: N, s2: D, l2: N, callSite: N) extends PushRule[N, D, Weight.NoWeight](s1, l1, s2, l2, callSite, Weight.NO_WEIGHT_ONE) {

    override def toString: String = {
        "<" + s1 + ";" + l1 + ">-><" + s2 + ";" + l2 + "." + callSite + ">"
    }
}