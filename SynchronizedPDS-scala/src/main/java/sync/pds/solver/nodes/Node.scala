package sync.pds.solver.nodes

import wpds.interfaces.State

class Node[Stmt, Fact](val stmt: Stmt, val variable: Fact) extends State {

    private var hashCode: Int = _

    def getStmt: Stmt = stmt

    def getFact: Fact = variable

    override def hashCode(): Int = {
        if (hashCode != 0) return hashCode
        val prime = 31
        var result = 1
        result = prime * result + (if (stmt == null) 0 else stmt.hashCode)
        result = prime * result + (if (variable == null) 0 else variable.hashCode)
        hashCode = result
        hashCode
    }

    override def equals(obj: Any): Boolean = obj match {
        case that: Node[_, _] =>
            if (this == that) return true
            if (that == null) return false
            if (getClass != that.getClass) return false
            if (stmt == null) {
                if (that.stmt != null) return false
            } else if (!stmt.equals(that.stmt)) return false
            if (variable == null) {
                if (that.variable != null) return false
            } else if (!variable.equals(that.variable)) return false
            true
        case _ => false
    }

    override def toString: String = "(" + variable + "," + stmt + ")"
}