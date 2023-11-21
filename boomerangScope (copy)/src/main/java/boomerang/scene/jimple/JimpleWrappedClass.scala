package boomerang.scene.jimple

import boomerang.scene.Method
import boomerang.scene.Type
import boomerang.scene.WrappedClass
import com.google.common.collect.Sets
import java.util
import soot.SootClass
import soot.SootMethod

class JimpleWrappedClass(private var delegate: Nothing) extends Nothing {
  private var methods: Nothing = null

  def getMethods: Nothing = {
    val ms = delegate.getMethods
    if (methods == null) {
      methods = Sets.newHashSet
      import scala.collection.JavaConversions._
      for (m <- ms) {
        if (m.hasActiveBody) methods.add(JimpleMethod.of(m))
      }
    }
    methods
  }

  def hasSuperclass: Boolean = delegate.hasSuperclass

  def getSuperclass = new JimpleWrappedClass(delegate.getSuperclass)

  def getType = new Nothing(delegate.getType)

  def isApplicationClass: Boolean = delegate.isApplicationClass

  @Override def toString: Nothing = delegate.toString

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
    val other = obj.asInstanceOf[JimpleWrappedClass]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def getFullyQualifiedName: Nothing = delegate.getName

  @Override def getName: Nothing = delegate.getName

  @Override def getDelegate: Nothing = delegate
}