package sync.pds.solver

import sync.pds.solver.nodes.Node
import wpds.interfaces.Location

trait SyncPDSUpdateListener[Stmt <: Location, Fact] {
    def onReachableNodeAdded(reachableNode: Node[Stmt, Fact]): Unit
}