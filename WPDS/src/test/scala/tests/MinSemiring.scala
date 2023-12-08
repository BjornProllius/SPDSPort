package tests

import wpds.impl.Weight
import wpds.interfaces.Location

class MinSemiring(var i: Int) extends Weight {

    def this() = this(0)

    override def extendWith(other: Weight): Weight = {
        if (other.equals(one)) return this
        if (this.equals(one)) return other
        val o = other.asInstanceOf[MinSemiring]
        new MinSemiring(o.i + i)
    }

    override def combineWith(other: Weight): Weight = {
        if (other.equals(zero)) return this
        if (this.equals(zero)) return other
        val o = other.asInstanceOf[MinSemiring]
        new MinSemiring(Math.min(o.i, i))
    }

    override def toString: String = i.toString

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + i
        result
    }

    override def equals(obj: Any): Boolean = {
        if (this == obj) return true
        if (obj == null) return false
        if (getClass != obj.getClass) return false
        val other = obj.asInstanceOf[MinSemiring]
        if (i != other.i) return false
        true
    }
}

object MinSemiring {
    private var one: MinSemiring = _

    def one[N <: Location]: MinSemiring = {
        if (one == null)
            one = new MinSemiring {
                override def toString: String = "<ONE>"
                override def equals(obj: Any): Boolean = obj == this
            }
        one
    }

    private var zero: MinSemiring = _

    def zero[N <: Location]: MinSemiring = {
        if (zero == null)
            zero = new MinSemiring {
                override def toString: String = "<ZERO>"
                override def equals(obj: Any): Boolean = obj == this
            }
        zero
    }
}