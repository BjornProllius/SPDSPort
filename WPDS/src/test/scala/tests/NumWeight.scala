package tests

import wpds.impl.Weight
import wpds.interfaces.Location

class NumWeight(var i: Int) extends Weight {

    def this() = this(0)

    override def extendWith(other: Weight): Weight = {
        if (this.equals(one)) return other
        if (other.equals(one)) return this
        if (this.equals(zero) || other.equals(zero)) return zero
        val o = other.asInstanceOf[NumWeight]
        new NumWeight(o.i + i)
    }

    override def combineWith(other: Weight): Weight = {
        if (other.equals(zero)) return this
        if (this.equals(zero)) return other
        val o = other.asInstanceOf[NumWeight]
        if (o.i == i) return o
        zero
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
        val other = obj.asInstanceOf[NumWeight]
        if (i != other.i) return false
        true
    }
}

object NumWeight {
    private var one: NumWeight = _

    def one[N <: Location]: NumWeight = {
        if (one == null)
            one = new NumWeight {
                override def toString: String = "<ONE>"
                override def equals(obj: Any): Boolean = obj == this
            }
        one
    }

    private var zero: NumWeight = _

    def zero[N <: Location]: NumWeight = {
        if (zero == null)
            zero = new NumWeight {
                override def toString: String = "<ZERO>"
                override def equals(obj: Any): Boolean = obj == this
            }
        zero
    }
}