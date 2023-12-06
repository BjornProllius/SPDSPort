package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State

trait NestedAutomatonListener[N <: Location, D <: State, W <: Weight] {
  def nestedAutomaton(parent: WeightedPAutomaton[N, D, W], child: WeightedPAutomaton[N, D, W]): Unit
}