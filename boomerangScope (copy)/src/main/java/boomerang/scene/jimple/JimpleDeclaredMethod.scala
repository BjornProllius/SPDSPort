package boomerang.scene.jimple

import boomerang.scene.DeclaredMethod
import boomerang.scene.InvokeExpr
import boomerang.scene.WrappedClass
import soot.SootMethod

class JimpleDeclaredMethod(inv: Nothing, private var delegate: Nothing) extends Nothing(inv) {
  @Override def isNative: Boolean = delegate.isNative

  @Override def getSubSignature: Nothing = delegate.getSubSignature

  @Override def toString: Nothing = delegate.toString

  @Override def getName: Nothing = delegate.getName

  @Override def isStatic: Boolean = delegate.isStatic

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (delegate == null) 0
    else delegate.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[JimpleDeclaredMethod]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def isConstructor: Boolean = delegate.isConstructor

  @Override def getSignature: Nothing = delegate.getSignature

  @Override def getDeclaringClass = new Nothing(delegate.getDeclaringClass)

  def getDelegate: Nothing = delegate
}