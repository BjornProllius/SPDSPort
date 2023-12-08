package boomerang.util

import boomerang.scene.{Field, Val}
import com.google.common.collect.Lists

import scala.collection.mutable

class AccessPath(val value: Val, fields: Collection[Field]) {
  private val val = value
  private val fieldChain = fields

  def this(value: Val) {
    this(value, Lists.newArrayList[Field]())
  }

  def this(value: Val, field: Field) {
    this(value, Lists.newArrayList(field))
  }

  override def toString: String = {
    val.toString + (if (fieldChain.isEmpty) "" else fieldChain.toString) + (if (isOverApproximated) "*" else "")
  }

  def isOverApproximated: Boolean = {
    fieldChain.isInstanceOf[Set[_]]
  }

  def getBase: Val = {
    this.val
  }

  def getFields: Collection[Field] = {
    fieldChain
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (fieldChain == null) 0 else fieldChain.hashCode)
    result = prime * result + (if (val == null) 0 else val.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: AccessPath =>
        (that canEqual this) &&
          fieldChain == that.fieldChain &&
          val == that.val
      case _ => false
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[AccessPath]
}