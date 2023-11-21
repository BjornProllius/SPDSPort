package boomerang.callgraph

import boomerang.WeightedBoomerang
import boomerang.scene.CallGraph
import boomerang.scene.InvokeExpr
import boomerang.scene.Method
import boomerang.scene.Statement
import java.util

object ICallerCalleeResolutionStrategy {
  trait Factory {
    def newInstance(solver: Nothing, cg: Nothing): ICallerCalleeResolutionStrategy
  }
}

trait ICallerCalleeResolutionStrategy {
  def computeFallback(observableDynamicICFG: Nothing): Unit

  def resolveSpecialInvoke(ie: Nothing): Nothing

  def resolveInstanceInvoke(stmt: Nothing): Nothing

  def resolveStaticInvoke(ie: Nothing): Nothing
}