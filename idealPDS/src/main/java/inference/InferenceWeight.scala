package inference

import boomerang.scene.Method
import scala.collection.mutable.Set

class InferenceWeight private (val invokedMethods: Set[Method], val rep: String) {
  def this(m: Method) = this(Set(m), null)

  override def extendWith(other: InferenceWeight): InferenceWeight = {
    if (other.equals(InferenceWeight.one())) return this
    if (this.equals(InferenceWeight.one())) return other
    if (other.equals(InferenceWeight.zero()) || this.equals(InferenceWeight.zero())) {
      return InferenceWeight.zero()
    }
    val otherInvokedMethods = other.invokedMethods
    val res = invokedMethods.clone()
    res.addAll(otherInvokedMethods)
    new InferenceWeight(res, null)
  }

  override def combineWith(other: InferenceWeight): InferenceWeight = extendWith(other)

  override def toString: String =
    if (rep != null) rep
    else s"{Func:${invokedMethods.toString}}"

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (rep == null) 0 else rep.hashCode)
    result = prime * result + (if (invokedMethods == null) 0 else invokedMethods.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (this == obj) return true
    if (obj == null || getClass != obj.getClass) return false
    val other = obj.asInstanceOf[InferenceWeight]
    if (rep == null) {
      if (other.rep != null) return false
    } else if (rep != other.rep) return false
    if (invokedMethods == null) {
      if (other.invokedMethods != null) return false
    } else if (invokedMethods != other.invokedMethods) return false
    true
  }
}

object InferenceWeight {
  private var oneInstance: InferenceWeight = _
  private var zeroInstance: InferenceWeight = _

  def one(): InferenceWeight = {
    if (oneInstance == null) oneInstance = new InferenceWeight("ONE", null)
    oneInstance
  }

  def zero(): InferenceWeight = {
    if (zeroInstance == null) zeroInstance = new InferenceWeight("ZERO", null)
    zeroInstance
  }
}
