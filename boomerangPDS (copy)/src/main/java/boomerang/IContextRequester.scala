package boomerang

import boomerang.scene.Statement
import java.util

trait IContextRequester {
  def getCallSiteOf(child: Nothing): Nothing

  def initialContext(stmt: Nothing): Nothing
}