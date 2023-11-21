package ideal

import boomerang.WeightedForwardQuery
import boomerang.results.ForwardBoomerangResults
import com.google.common.collect.Maps
import java.util
import wpds.impl.Weight

class StoreIDEALResultHandler[W <: Weight] extends Nothing {
  private[ideal] val seedToSolver = Maps.newHashMap

  @Override def report(seed: Nothing, res: Nothing): Unit = {
    seedToSolver.put(seed, res)
  }

  def getResults: Nothing = seedToSolver
}