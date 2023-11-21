package boomerang.scene.jimple

import boomerang.scene.Method
import boomerang.scene.Val
import boomerang.scene.ValWithFalseVariable
import soot.Value

class JimpleDoubleVal(v: Nothing, m: Nothing, private val falseVariable: Nothing) extends Nothing(v, m) with Nothing {
  def getFalseVariable: Nothing = falseVariable

  @Override def toString: Nothing = "Instanceof " + falseVariable + " " + super.toString

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (falseVariable == null) 0
    else falseVariable.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[JimpleDoubleVal]
    if (falseVariable == null) if (other.falseVariable != null) return false
    else if (!falseVariable.equals(other.falseVariable)) return false
    true
  }
}