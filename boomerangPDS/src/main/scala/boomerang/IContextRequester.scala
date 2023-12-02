package boomerang

import boomerang.scene.Statement

trait IContextRequester {
  def getCallSiteOf(child: Context): Collection[Context]

  def initialContext(stmt: Statement): Context
}