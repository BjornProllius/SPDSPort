package wpds.impl

import wpds.interfaces.{Location, State}

abstract class Rule[N <: Location, D <: State, W <: Weight](var s1: D, var l1: N, var s2: D, var l2: N, var w: W) {

    def getStartConfig: Configuration[N, D] = new Configuration[N, D](l1, s1)

    def getTargetConfig: Configuration[N, D] = new Configuration[N, D](l2, s2)

    def getL1: N = l1

    def getL2: N = l2

    def getS1: D = s1

    def getS2: D = s2

    def setS1(s1: D): Unit = this.s1 = s1

    def getWeight: W = w

    override def hashCode(): Int = {
        val prime = 31
        var result = 1
        result = prime * result + (if (l1 == null) 0 else l1.hashCode())
        result = prime * result + (if (l2 == null) 0 else l2.hashCode())
        result = prime * result + (if (s1 == null) 0 else s1.hashCode())
        result = prime * result + (if (s2 == null) 0 else s2.hashCode())
        result = prime * result + (if (w == null) 0 else w.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case other: Rule[_, _, _] =>
            (this eq other) || (other != null && getClass == other.getClass &&
                l1 == other.l1 && l2 == other.l2 && s1 == other.s1 && s2 == other.s2 && w == other.w)
        case _ => false
    }
}