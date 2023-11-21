package boomerang.scene

abstract class DeclaredMethod(private var inv: Nothing) {
  def isNative: Boolean

  def getSubSignature: Nothing

  def getName: Nothing

  def isStatic: Boolean

  def isConstructor: Boolean

  def getSignature: Nothing

  def getDeclaringClass: Nothing

  def getInvokeExpr: Nothing = inv
}