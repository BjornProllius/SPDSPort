package sync.pds.solver.nodes

class ExclusionNode[Stmt, Fact, Location](stmt: Stmt, variable: Fact, var exclusion: Location) extends Node[Stmt, Fact](stmt, variable) {

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + (if (exclusion == null) 0 else exclusion.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: ExclusionNode[_, _, _] =>
            if (this == that) return true
            if (!super.equals(that)) return false
            if (getClass != that.getClass) return false
            if (exclusion == null) {
                if (that.exclusion != null) return false
            } else if (!exclusion.equals(that.exclusion)) return false
            true
        case _ => false
    }

    def exclusion(): Location = exclusion
}