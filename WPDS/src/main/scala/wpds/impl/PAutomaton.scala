package wpds.impl

import pathexpression.LabeledGraph
import wpds.impl.Weight.NoWeight
import wpds.interfaces.Location
import wpds.interfaces.State

abstract class PAutomaton[N <: Location, D <: State]
    extends WeightedPAutomaton[N, D, NoWeight] with LabeledGraph[D, N] {

  override def getOne: NoWeight = NoWeight.NO_WEIGHT_ONE
}