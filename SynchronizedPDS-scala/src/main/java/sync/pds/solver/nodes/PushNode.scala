package sync.pds.solver.nodes

import sync.pds.solver.SyncPDSSolver.PDSSystem

class PushNode[Stmt, Fact, Location](stmt: Stmt, variable: Fact, var location: Location, var system: PDSSystem) extends Node[Stmt, Fact](stmt, variable) {

    def system(): PDSSystem = system

    def location(): Location = location

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + (if (location == null) 0 else location.hashCode())
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: PushNode[_, _, _] =>
            if (this == that) return true
            if (!super.equals(that)) return false
            if (getClass != that.getClass) return false
            if (location == null) {
                if (that.location != null) return false
            } else if (!location.equals(that.location)) return false
            true
        case _ => false
    }

    override def toString: String = super.toString + " Push " + location
}