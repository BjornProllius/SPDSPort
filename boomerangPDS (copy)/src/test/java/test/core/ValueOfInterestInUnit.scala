package test.core

import boomerang.Query
import boomerang.scene.ControlFlowGraph.Edge
import java.util.Optional

trait ValueOfInterestInUnit {
  def test(cfgEdge: Nothing): Nothing
}