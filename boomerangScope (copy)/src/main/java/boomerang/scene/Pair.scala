package boomerang.scene

class Pair[X, Y](private val x: X, private val y: Y) {
  def getX: X = x

  def getY: Y = y

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (x == null) 0
    else x.hashCode)
    result = prime * result + (if (y == null) 0
    else y.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Pair[_, _]]
    if (x == null) if (other.x != null) return false
    else if (!x.equals(other.x)) return false
    if (y == null) if (other.y != null) return false
    else if (!y.equals(other.y)) return false
    true
  }

  @Override def toString: Nothing = "Pair(" + x + "," + y + ")"
}