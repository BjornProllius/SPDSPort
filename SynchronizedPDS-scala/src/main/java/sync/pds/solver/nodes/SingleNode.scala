package sync.pds.solver.nodes

class SingleNode[Fact](private var fact: Fact) extends INode[Fact] {
    private var hashCode: Int = 0

    override def hashCode(): Int = {
        if (hashCode != 0) return hashCode
        val prime = 31
        var result = 1
        result = prime * result + (if (fact == null) 0 else fact.hashCode())
        hashCode = result
        hashCode
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: SingleNode[_] =>
            if (this == that) return true
            if (that == null) return false
            if (getClass != that.getClass) return false
            if (fact == null) {
                if (that.fact != null) return false
            } else if (!fact.equals(that.fact)) return false
            true
        case _ => false
    }

    override def fact(): Fact = fact

    override def toString: String = fact.toString
}