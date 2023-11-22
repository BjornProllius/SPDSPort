package sync.pds.solver.nodes

import sync.pds.solver.SyncPDSSolver.PDSSystem

class CallPopNode[Location, Stmt](location: Location, system: PDSSystem, val returnSite: Stmt) extends PopNode[Location](location, system) {

    def getReturnSite: Stmt = returnSite

    override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + (if (returnSite == null) 0 else returnSite.hashCode)
        result
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: CallPopNode[_, _] =>
            if (this == that) return true
            if (!super.equals(that)) return false
            if (getClass != that.getClass) return false
            if (returnSite == null) {
                if (that.returnSite != null) return false
            } else if (!returnSite.equals(that.returnSite)) return false
            true
        case _ => false
    }
}