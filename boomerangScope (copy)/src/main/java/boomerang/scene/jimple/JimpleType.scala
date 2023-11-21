package boomerang.scene.jimple

import boomerang.scene.AllocVal
import boomerang.scene.Type
import boomerang.scene.Val
import boomerang.scene.WrappedClass
import soot.ArrayType
import soot.BooleanType
import soot.NullType
import soot.PrimType
import soot.RefType
import soot.Scene
import soot.SootClass

class JimpleType(private var delegate: Nothing) extends Nothing {
  def isNullType: Boolean = delegate.isInstanceOf[Nothing]

  def isRefType: Boolean = delegate.isInstanceOf[Nothing]

  @Override def isBooleanType: Boolean = delegate.isInstanceOf[Nothing]

  def isArrayType: Boolean = delegate.isInstanceOf[Nothing]

  def getArrayBaseType = new JimpleType(delegate.asInstanceOf[Nothing].baseType)

  def getWrappedClass = new Nothing(delegate.asInstanceOf[Nothing].getSootClass)

  def getDelegate: Nothing = delegate

  @Override def doesCastFail(targetVal: Nothing, target: Nothing): Boolean = {
    val targetType = targetVal.asInstanceOf[JimpleType].getDelegate.asInstanceOf[Nothing]
    val sourceType = this.getDelegate.asInstanceOf[Nothing]
    if (targetType.getSootClass.isPhantom || sourceType.getSootClass.isPhantom) return false
    if (target.isInstanceOf[Nothing] && target.asInstanceOf[Nothing].getAllocVal.isNewExpr) {
      val castFails = Scene.v.getOrMakeFastHierarchy.canStoreType(targetType, sourceType)
      return !castFails
    }
    // TODO this line is necessary as canStoreType does not properly work for
    // interfaces, see Java doc.
    if (targetType.getSootClass.isInterface) return false
    val castFails = Scene.v.getOrMakeFastHierarchy.canStoreType(targetType, sourceType) || Scene.v.getOrMakeFastHierarchy.canStoreType(sourceType, targetType)
    !castFails
  }

  def isSubtypeOf(`type`: Nothing): Boolean = {
    val interfaceType = Scene.v.getSootClass(`type`)
    if (delegate.toString.equals(`type`)) return true
    if (!delegate.isInstanceOf[Nothing]) {
      if (delegate.isInstanceOf[Nothing]) return true
      if (delegate.isInstanceOf[Nothing]) return `type`.equals(delegate.toString)
      throw new Nothing("More")
    }
    val allocatedType = delegate.asInstanceOf[Nothing]
    if (!interfaceType.isInterface) return Scene.v.getFastHierarchy.isSubclass(allocatedType.getSootClass, interfaceType)
    if (Scene.v.getActiveHierarchy.getSubinterfacesOfIncluding(interfaceType).contains(allocatedType.getSootClass)) return true
    Scene.v.getActiveHierarchy.getImplementersOf(interfaceType).contains(allocatedType.getSootClass)
  }

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
    val other = obj.asInstanceOf[JimpleType]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def toString: Nothing = delegate.toString
}