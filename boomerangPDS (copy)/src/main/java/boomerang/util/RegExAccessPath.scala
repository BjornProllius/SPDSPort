package boomerang.util

import boomerang.scene.Field
import boomerang.scene.Val
import pathexpression.IRegEx

class RegExAccessPath(private val `val`: Nothing, private val fields: Nothing) {
  def getVal: Nothing = `val`

  def getFields: Nothing = fields

  @Override def toString: Nothing = `val` + " " + fields.toString

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (fields == null) 0
    else fields.hashCode)
    result = prime * result + (if (`val` == null) 0
    else `val`.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[RegExAccessPath]
    if (fields == null) if (other.fields != null) return false
    else if (!fields.equals(other.fields)) return false
    if (`val` == null) if (other.`val` != null) return false
    else if (!`val`.equals(other.`val`)) return false
    true
  }
}