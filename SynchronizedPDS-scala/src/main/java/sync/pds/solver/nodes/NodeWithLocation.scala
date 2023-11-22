package sync.pds.solver.nodes

class NodeWithLocation[Stmt, Fact, Location](stmt: Stmt, variable: Fact, var loc: Location) extends INode[Node[Stmt, Fact]] {

    private var fact: Node[Stmt, Fact] = new Node[Stmt, Fact](stmt, variable)

    override def fact(): Node[Stmt, Fact] = fact

    def location(): Location = loc

    override def hashCode(): Int = {
        val prime = 31
        var result = 1
        result = prime * result + (if (fact == null) 0 else fact.hashCode())
        result = prime * result + (if (loc == null) 0 else loc.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: NodeWithLocation[_, _, _] =>
            if (this == that) return true
            if (that == null) return false
            if (getClass != that.getClass) return false
            if (fact == null) {
                if (that.fact != null) return false
            } else if (!fact.equals(that.fact)) return false
            if (loc == null) {
                if (that.loc != null) return false
            } else if (!loc.equals(that.loc)) return false
            true
        case _ => false
    }

    override def toString: String = fact + " loc: " + loc
}