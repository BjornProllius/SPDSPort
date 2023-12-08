package ideal

import boomerang.{WeightedForwardQuery, results => BoomerangResults}
import com.google.common.collect.{Maps => JMaps}
import wpds.impl.Weight

import scala.collection.mutable

class StoreIDEALResultHandler[W <: Weight] extends IDEALResultHandler[W] {
  private val seedToSolver: mutable.Map[WeightedForwardQuery[W], BoomerangResults.ForwardBoomerangResults[W]] =
    JMaps.newHashMap[WeightedForwardQuery[W], BoomerangResults.ForwardBoomerangResults[W]]()

  override def report(seed: WeightedForwardQuery[W], res: BoomerangResults.ForwardBoomerangResults[W]): Unit = {
    seedToSolver.put(seed, res)
  }

  def getResults: Map[WeightedForwardQuery[W], BoomerangResults.ForwardBoomerangResults[W]] = seedToSolver.toMap
}
