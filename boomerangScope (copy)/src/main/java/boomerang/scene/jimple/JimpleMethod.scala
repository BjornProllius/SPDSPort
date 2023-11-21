package boomerang.scene.jimple

import boomerang.scene.ControlFlowGraph
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.WrappedClass
import com.google.common.collect.Interner
import com.google.common.collect.Interners
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import soot.Local
import soot.SootMethod
import soot.util.Chain

object JimpleMethod {
  private val INTERNAL_POOL = Interners.newWeakInterner

  def of(m: Nothing): JimpleMethod = INTERNAL_POOL.intern(new JimpleMethod(m))
}

class JimpleMethod private(private val delegate: Nothing) extends Nothing {
  if (!delegate.hasActiveBody) throw new Nothing("Building a Jimple method without active body present")
  private var cfg: Nothing = null
  private var parameterLocalCache: Nothing = null
  private var localCache: Nothing = null

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
    val other = obj.asInstanceOf[JimpleMethod]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  @Override def toString: Nothing = if (delegate != null) delegate.toString
  else "METHOD EPS"

  def isStaticInitializer: Boolean = delegate.isStaticInitializer

  def isParameterLocal(`val`: Nothing): Boolean = {
    if (`val`.isStatic) return false
    if (!delegate.hasActiveBody) throw new Nothing("Soot Method has no active body")
    val parameterLocals = getParameterLocals
    parameterLocals.contains(`val`)
  }

  def isThisLocal(`val`: Nothing): Boolean = {
    if (`val`.isStatic) return false
    if (!delegate.hasActiveBody) throw new Nothing("Soot Method has no active body")
    if (delegate.isStatic) return false
    val parameterLocals = getThisLocal
    parameterLocals.equals(`val`)
  }

  def getLocals: Nothing = {
    if (localCache == null) {
      localCache = Sets.newHashSet
      val locals = delegate.getActiveBody.getLocals
      import scala.collection.JavaConversions._
      for (l <- locals) {
        localCache.add(new Nothing(l, this))
      }
    }
    localCache
  }

  def getThisLocal = new Nothing(delegate.getActiveBody.getThisLocal, this)

  def getParameterLocals: Nothing = {
    if (parameterLocalCache == null) {
      parameterLocalCache = Lists.newArrayList
      import scala.collection.JavaConversions._
      for (v <- delegate.getActiveBody.getParameterLocals) {
        parameterLocalCache.add(new Nothing(v, this))
      }
    }
    parameterLocalCache
  }

  def isStatic: Boolean = delegate.isStatic

  def isNative: Boolean = delegate.isNative

  def getStatements: Nothing = getControlFlowGraph.getStatements

  def getDeclaringClass = new Nothing(delegate.getDeclaringClass)

  def getControlFlowGraph: Nothing = {
    if (cfg == null) cfg = new Nothing(this)
    cfg
  }

  def getSubSignature: Nothing = delegate.getSubSignature

  def getDelegate: Nothing = delegate

  def getName: Nothing = delegate.getName

  @Override def isConstructor: Boolean = delegate.isConstructor

  @Override def isPublic: Boolean = delegate.isPublic
}