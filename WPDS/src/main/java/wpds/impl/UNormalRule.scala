package wpds.impl

import wpds.interfaces.{Location, State}

class UNormalRule[N <: Location, D <: State](s1: D, l1: N, s2: D, l2: N) 
    extends NormalRule[N, D, NoWeight](s1, l1, s2, l2, NoWeight.NO_WEIGHT_ONE) {

    override def toString: String = s"<$s1;$l1>-><$s2;$l2>"
}