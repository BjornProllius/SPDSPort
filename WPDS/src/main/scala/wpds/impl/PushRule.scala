package wpds.impl

import wpds.interfaces.{Location, State}
import wpds.impl.Weight

class PushRule[N <: Location, D <: State, W <: Weight](s1: D, l1: N, s2: D, l2: N, callSite: N, w: W) extends Rule[N, D, W](s1, l1, s2, l2, w) {

    protected var _callSite: N = callSite

    def getCallSite: N = _callSite

    override def hashCode: Int = {
        val prime = 31
        var result = super.hashCode
        result = prime * result + (if (_callSite == null) 0 else _callSite.hashCode)
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: PushRule[N, D, W] =>
            (this eq that) || (that canEqual this) && (super.equals(that)) && (_callSite == null && that._callSite == null || _callSite != null && _callSite == that._callSite)
        case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[PushRule[N, D, W]]

    override def toString: String = "<" + s1 + ";" + l1 + ">-><" + s2 + ";" + l2 + "." + _callSite + ">(" + w + ")"
}