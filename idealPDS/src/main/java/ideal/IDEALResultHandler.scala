package ideal

import boomerang.{WeightedForwardQuery, ForwardBoomerangResults}
import wpds.impl.Weight

class IDEALResultHandler[W <: Weight] {
  def report(seed: WeightedForwardQuery[W], res: ForwardBoomerangResults[W]): Unit = {}
}
