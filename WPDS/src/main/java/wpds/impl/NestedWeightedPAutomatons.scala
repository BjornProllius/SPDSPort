package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State

trait NestedWeightedPAutomatons[N <: Location, D <: State, W <: Weight] {

  def putSummaryAutomaton(target: D, aut: WeightedPAutomaton[N, D, W]): Unit

  def getSummaryAutomaton(target: D): WeightedPAutomaton[N, D, W]
}