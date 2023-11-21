package boomerang.scene

import java.util

trait InvokeExpr {
  def getArg(index: Int): Nothing

  def getArgs: Nothing

  def isInstanceInvokeExpr: Boolean

  def getBase: Nothing

  def getMethod: Nothing

  def isSpecialInvokeExpr: Boolean

  def isStaticInvokeExpr: Boolean
}