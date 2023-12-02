package boomerang.weights

import wpds.impl.Weight

class MinDistanceWeight private (var minDistance: Integer, var rep: String) extends Weight {

  def this(minDistance: Integer) {
    this(minDistance, null)
  }

  override def extendWith(o: Weight): Weight = {
    if (!o.isInstanceOf[MinDistanceWeight])
      throw new RuntimeException("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[MinDistanceWeight]
    if (other == MinDistanceWeight.one) return this
    if (this == MinDistanceWeight.one) return other
    val newDistance = minDistance + other.minDistance
    new MinDistanceWeight(newDistance)
  }

  override def combineWith(o: Weight): Weight = {
    if (!o.isInstanceOf[MinDistanceWeight])
      throw new RuntimeException("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[MinDistanceWeight]
    if (other == MinDistanceWeight.one) return this
    if (this == MinDistanceWeight.one) return other
    new MinDistanceWeight(Math.min(other.minDistance, minDistance))
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (minDistance == null) 0 else minDistance.hashCode)
    result = prime * result + (if (rep == null) 0 else rep.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: MinDistanceWeight =>
        (that canEqual this) &&
          minDistance == that.minDistance &&
          rep == that.rep
      case _ => false
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MinDistanceWeight]

  override def toString: String = {
    if (this == MinDistanceWeight.one) "ONE " else " Distance: " + minDistance
  }

  def getMinDistance: Integer = minDistance
}

object MinDistanceWeight {
  private var one: MinDistanceWeight = _

  def one: MinDistanceWeight = {
    if (one == null) one = new MinDistanceWeight(null, "ONE")
    one
  }
}