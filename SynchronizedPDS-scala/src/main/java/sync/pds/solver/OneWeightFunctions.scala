package sync.pds.solver

import wpds.impl.Weight

class OneWeightFunctions[Stmt, Fact, Field, W <: Weight](one: W) extends WeightFunctions[Stmt, Fact, Field, W] {

    override def push(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact], field: Field): W = one

    override def normal(curr: Node[Stmt, Fact], succ: Node[Stmt, Fact]): W = one

    override def pop(curr: Node[Stmt, Fact]): W = one

    override def getOne(): W = one
}