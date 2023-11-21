package boomerang.scene

trait Type {
  def isNullType: Boolean

  def isRefType: Boolean

  def isArrayType: Boolean

  def getArrayBaseType: Type

  def getWrappedClass: Nothing

  def doesCastFail(targetVal: Type, target: Nothing): Boolean

  def isSubtypeOf(`type`: Nothing): Boolean

  def isBooleanType: Boolean
}