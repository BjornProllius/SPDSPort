package sync.pds.solver

import sync.pds.solver.nodes.Node
import wpds.impl.Weight

trait WeightFunctions[Stmt, Fact, Field, W <: Weight] {
    def push(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact], field: Field): W

    def normal(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact]): W

    def pop(curr: Node[Stmt, Fact]): W

    def getOne(): W
}