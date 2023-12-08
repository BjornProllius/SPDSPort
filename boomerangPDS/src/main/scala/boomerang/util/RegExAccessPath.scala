package boomerang.util

import boomerang.scene.Field
import boomerang.scene.Val
import pathexpression.IRegEx

class RegExAccessPath(val value: Val, val fields: IRegEx[Field]) {

  override def toString: String = value + " " + fields.toString

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (fields == null) 0 else fields.hashCode)
    result = prime * result + (if (value == null) 0 else value.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: RegExAccessPath =>
        (that canEqual this) &&
          fields == that.fields &&
          value == that.value
      case _ => false
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[RegExAccessPath]
}