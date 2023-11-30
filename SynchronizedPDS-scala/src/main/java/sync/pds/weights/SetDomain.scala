package sync.pds.weights

import com.google.common.collect.Sets
import sync.pds.solver.nodes.Node
import wpds.impl.Weight
import wpds.interfaces.Location

import scala.collection.JavaConverters._

class SetDomain[N, Stmt, Fact](private var nodes: Collection[Node[Stmt, Fact]], private val rep: String) extends Weight {

    def this(rep: String) = this(null, rep)

    def this(nodes: Collection[Node[Stmt, Fact]]) = this(nodes, null)

    def this(node: Node[Stmt, Fact]) = this(Sets.newHashSet(node), null)

    override def extendWith(other: Weight): Weight = {
        if (other == SetDomain.one) this
        else if (this == SetDomain.one) other
        else SetDomain.zero
    }

    override def combineWith(other: Weight): Weight = {
        if (other == SetDomain.zero) return this
        if (this == SetDomain.zero) return other
        if (this == SetDomain.one || other == SetDomain.one) return SetDomain.one
        other match {
            case that: SetDomain[N, Stmt, Fact] =>
                val merged = Sets.newHashSet(nodes)
                merged.addAll(that.nodes)
                new SetDomain[N, Stmt, Fact](merged)
            case _ => SetDomain.zero
        }
    }

    override def toString: String = {
        if (rep != null) rep
        else nodes.toString
    }

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + (if (nodes == null) 0 else nodes.hashCode)
        result = prime * result + (if (rep == null) 0 else rep.hashCode)
        result
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case that: SetDomain[N, Stmt, Fact] =>
                (that canEqual this) &&
                    nodes == that.nodes &&
                    rep == that.rep
            case _ => false
        }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[SetDomain[N, Stmt, Fact]]

    def elements: Set[Node[Stmt, Fact]] = Sets.newHashSet(nodes).asScala.toSet
}

object SetDomain {
    private var one: SetDomain[_, _, _] = _
    private var zero: SetDomain[_, _, _] = _

    def one[N <: Location, Stmt, Fact]: SetDomain[N, Stmt, Fact] = {
        if (one == null) one = new SetDomain("<1>")
        one.asInstanceOf[SetDomain[N, Stmt, Fact]]
    }

    def zero[N <: Location, Stmt, Fact]: SetDomain[N, Stmt, Fact] = {
        if (zero == null) zero = new SetDomain("<0>")
        zero.asInstanceOf[SetDomain[N, Stmt, Fact]]
    }
}