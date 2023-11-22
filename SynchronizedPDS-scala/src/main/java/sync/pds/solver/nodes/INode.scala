package sync.pds.solver.nodes

import wpds.interfaces.State

trait INode[Fact] extends State {
    def fact(): Fact
}