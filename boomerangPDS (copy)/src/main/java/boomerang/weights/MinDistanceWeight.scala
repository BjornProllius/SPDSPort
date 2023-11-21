package boomerang.weights

import wpds.impl.Weight

object MinDistanceWeight {
  private var one: MinDistanceWeight = null
  private val zero: MinDistanceWeight = null

  def one: MinDistanceWeight = {
    if (one == null) one = new MinDistanceWeight("ONE")
    one
  }
}

class MinDistanceWeight extends Nothing {
  private var minDistance = -1
  private var rep: Nothing = null

  def this(rep: Nothing) {
    this()
    this.rep = rep
  }

  def this(minDistance: Nothing) {
    this()
    this.minDistance = minDistance
  }

  @Override def extendWith(o: Nothing): Nothing = {
    if (!o.isInstanceOf[MinDistanceWeight]) throw new Nothing("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[MinDistanceWeight]
    if (other == MinDistanceWeight.one) return this
    if (this == MinDistanceWeight.one) return other
    val newDistance = minDistance + other.minDistance
    new MinDistanceWeight(newDistance)
  }

  @Override def combineWith(o: Nothing): Nothing = {
    if (!o.isInstanceOf[MinDistanceWeight]) throw new Nothing("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[MinDistanceWeight]
    if (other == MinDistanceWeight.one) return this
    if (this == MinDistanceWeight.one) return other
    new MinDistanceWeight(Math.min(other.minDistance, minDistance))
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (minDistance == null) 0
    else minDistance.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[MinDistanceWeight]
    if (minDistance == null) if (other.minDistance != null) return false
    else if (!minDistance.equals(other.minDistance)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    true
  }

  @Override def toString: Nothing = if (this == MinDistanceWeight.one) "ONE "
  else " Distance: " + minDistance

  def getMinDistance: Nothing = minDistance
}