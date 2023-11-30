package sync.pds.solver.nodes

class GeneratedState[L, N](var node: INode[L], var loc: N) extends INode[L] {

    override def fact(): L = node.fact()

    def node(): INode[L] = node

    def location(): N = loc

    override def toString: String = node + " " + loc

    override def hashCode(): Int = {
        val prime = 31
        var result = 1
        result = prime * result + (if (loc == null) 0 else loc.hashCode)
        result = prime * result + (if (node == null) 0 else node.hashCode)
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: GeneratedState[_, _] =>
            if (this == that) return true
            if (that == null) return false
            if (getClass != that.getClass) return false
            if (loc == null) {
                if (that.loc != null) return false
            } else if (!loc.equals(that.loc)) return false
            if (node == null) {
                if (that.node != null) return false
            } else if (!node.equals(that.node)) return false
            true
        case _ => false
    }
}