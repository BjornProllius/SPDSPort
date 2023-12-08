package wpds.impl

import wpds.interfaces.{Location, State}

class UPopRule[N <: Location, D <: State](s1: D, l1: N, s2: D) 
    extends PopRule[N, D, NoWeight](s1, l1, s2, NoWeight.NO_WEIGHT_ONE) {

    override def toString: String = s"<$s1;$l1>-><$s2>"
}