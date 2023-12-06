package wpds.impl

import pathexpression.Edge
import wpds.interfaces.{Location, State}
import wpds.wildcard.Wildcard

class Transition[N <: Location, D <: State](val s1: D, val l1: N, val s2: D) extends Edge[D, N] {
    require(s1 != null)
    require(s2 != null)
    require(l1 != null)
    if (l1.isInstanceOf[Wildcard]) throw new RuntimeException("No wildcards allowed!")

    private var hashCode: Int = _

    def getStartConfig: Configuration[N, D] = new Configuration[N, D](l1, s1)

    def getTarget: D = s2

    def getStart: D = s1

    override def hashCode(): Int = {
        if (hashCode != 0) hashCode
        else {
            val prime = 31
            var result = 1
            result = prime * result + (if (l1 == null) 0 else l1.hashCode())
            result = prime * result + (if (s1 == null) 0 else s1.hashCode())
            result = prime * result + (if (s2 == null) 0 else s2.hashCode())
            hashCode = result
            hashCode
        }
    }

    override def equals(obj: Any): Boolean = obj match {
        case other: Transition[_, _] =>
            (this eq other) || (other != null && getClass == other.getClass &&
                l1 == other.l1 && s1 == other.s1 && s2 == other.s2)
        case _ => false
    }

    override def toString: String = s"$s1~$l1~>$s2"

    override def getLabel: N = l1
}