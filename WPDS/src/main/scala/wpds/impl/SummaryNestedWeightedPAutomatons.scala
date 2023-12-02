package wpds.impl

import scala.collection.mutable

class SummaryNestedWeightedPAutomatons[N <: Location, D <: State, W <: Weight] 
    extends NestedWeightedPAutomatons[N, D, W] {

    private val summaries = mutable.Map[D, WeightedPAutomaton[N, D, W]]()

    override def putSummaryAutomaton(target: D, aut: WeightedPAutomaton[N, D, W]): Unit = {
        summaries.put(target, aut)
    }

    override def getSummaryAutomaton(target: D): WeightedPAutomaton[N, D, W] = {
        summaries(target)
    }
}