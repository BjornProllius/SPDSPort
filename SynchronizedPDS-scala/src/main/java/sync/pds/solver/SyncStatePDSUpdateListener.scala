package sync.pds.solver

import sync.pds.solver.nodes.Node
import wpds.interfaces.Location

abstract class SyncStatePDSUpdateListener[Stmt <: Location, Fact](var node: Node[Stmt, Fact]) {

    def reachable(): Unit

    def getNode: Node[Stmt, Fact] = node
}