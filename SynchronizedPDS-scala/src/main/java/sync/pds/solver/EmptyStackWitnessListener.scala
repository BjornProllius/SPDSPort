package sync.pds.solver

import sync.pds.solver.nodes.Node

trait EmptyStackWitnessListener[Stmt, Fact] {
    def witnessFound(targetFact: Node[Stmt, Fact]): Unit
}