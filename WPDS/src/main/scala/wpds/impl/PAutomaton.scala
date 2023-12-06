package wpds.impl

import pathexpression.LabeledGraph
import wpds.interfaces.{Location, State}
import wpds.impl.Weight

abstract class PAutomaton[N <: Location, D <: State] extends WeightedPAutomaton[N, D, Weight.NoWeight] with LabeledGraph[D, N] {

  override def getOne: Weight.NoWeight = Weight.NO_WEIGHT_ONE
}