package boomerang.callgraph

import boomerang.WeightedBoomerang
import boomerang.scene.{CallGraph, InvokeExpr, Method, Statement}

trait ICallerCalleeResolutionStrategy {

  trait Factory {
    def newInstance(solver: WeightedBoomerang, cg: CallGraph): ICallerCalleeResolutionStrategy
  }

  def computeFallback(observableDynamicICFG: ObservableDynamicICFG): Unit

  def resolveSpecialInvoke(ie: InvokeExpr): Method

  def resolveInstanceInvoke(stmt: Statement): Collection[Method]

  def resolveStaticInvoke(ie: InvokeExpr): Method
}