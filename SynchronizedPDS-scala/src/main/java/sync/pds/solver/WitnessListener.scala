package sync.pds.solver

import sync.pds.solver.nodes.{INode, Node}
import wpds.impl.Transition
import wpds.interfaces.Location

trait WitnessListener[Stmt <: Location, Fact, Field <: Location] {

    def fieldWitness(transition: Transition[Field, INode[Node[Stmt, Fact]]]): Unit

    def callWitness(t: Transition[Stmt, INode[Fact]]): Unit
}