package boomerang.scene.jimple

import boomerang.scene.DeclaredMethod
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Val
import com.google.common.collect.Lists
import java.util
import soot.jimple.InstanceInvokeExpr
import soot.jimple.SpecialInvokeExpr
import soot.jimple.StaticInvokeExpr

class JimpleInvokeExpr(private var delegate: Nothing, private var m: Nothing) extends Nothing {
  private var cache: Nothing = null

  def getArg(index: Int): Nothing = {
    if (delegate.getArg(index) == null) return Val.zero
    new Nothing(delegate.getArg(index), m)
  }

  def getArgs: Nothing = {
    if (cache == null) {
      cache = Lists.newArrayList
      var i = 0
      while (i < delegate.getArgCount) {
        cache.add(getArg(i))
        i += 1
      }
    }
    cache
  }

  def isInstanceInvokeExpr: Boolean = delegate.isInstanceOf[Nothing]

  def getBase: Nothing = {
    val iie = delegate.asInstanceOf[Nothing]
    new Nothing(iie.getBase, m)
  }

  def getMethod = new Nothing(this, delegate.getMethod)

  def isSpecialInvokeExpr: Boolean = delegate.isInstanceOf[Nothing]

  def isStaticInvokeExpr: Boolean = delegate.isInstanceOf[Nothing]

  @Override def toString: Nothing = delegate.toString
}