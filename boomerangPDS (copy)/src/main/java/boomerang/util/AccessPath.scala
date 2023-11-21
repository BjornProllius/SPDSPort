package boomerang.util

import boomerang.scene.Field
import boomerang.scene.Val
import com.google.common.collect.Lists
import java.util

class AccessPath {
  final private var `val`: Nothing = null
  final private var fieldChain: Nothing = null

  def this(value: Nothing) {
    this()
    this.`val` = value
    this.fieldChain = Lists.newArrayList
  }

  def this(value: Nothing, field: Nothing) {
    this()
    this.`val` = value
    this.fieldChain = Lists.newArrayList(field)
  }

  def this(value: Nothing, fields: Nothing) {
    this()
    this.`val` = value
    this.fieldChain = fields
  }

  @Override def toString: Nothing = `val`.toString + "" + (if (fieldChain.isEmpty) ""
  else fieldChain.toString) + (if (isOverApproximated) "*"
  else "")

  def isOverApproximated: Boolean = fieldChain.isInstanceOf[Nothing]

  def getBase: Nothing = this.`val`

  def getFields: Nothing = fieldChain

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (fieldChain == null) 0
    else fieldChain.hashCode)
    result = prime * result + (if (`val` == null) 0
    else `val`.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[AccessPath]
    if (fieldChain == null) if (other.fieldChain != null) return false
    else if (!fieldChain.equals(other.fieldChain)) return false
    if (`val` == null) if (other.`val` != null) return false
    else if (!`val`.equals(other.`val`)) return false
    true
  }
}