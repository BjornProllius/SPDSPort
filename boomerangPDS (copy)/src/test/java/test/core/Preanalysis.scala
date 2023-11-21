package test.core

import boomerang.Query
import boomerang.scene.AnalysisScope
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import java.util
import java.util.Collections

class Preanalysis(cg: Nothing, private var f: Nothing) extends Nothing(cg) {
  @Override protected def generate(seed: Nothing): Nothing = {
    if (f.test(seed).isPresent) return Collections.singleton(f.test(seed).get)
    Collections.emptySet
  }
}