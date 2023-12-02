package wpds.impl

import scala.collection.mutable.{LinkedList, Set}
import wpds.interfaces.{IPushdownSystem, Location, State}
import wpds.wildcard.Wildcard



class PreStar[N <: Location, D <: State, W <: Weight] {
  private var worklist: LinkedList[Transition[N, D]] = LinkedList()
  private var pds: IPushdownSystem[N, D, W] = _
  private var fa: WeightedPAutomaton[N, D, W] = _

  def prestar(pds: IPushdownSystem[N, D, W], initialAutomaton: WeightedPAutomaton[N, D, W]): WeightedPAutomaton[N, D, W] = {
    this.pds = pds
    worklist = LinkedList(initialAutomaton.getTransitions: _*)
    fa = initialAutomaton

    for (trans <- fa.getTransitions.toSet) {
      val one = fa.getOne
      fa.addWeightForTransition(trans, one)
    }
    for (r <- pds.getPopRules) {
      update(Transition(r.getS1, r.getL1, r.getS2), r.getWeight, LinkedList())
    }

    while (worklist.nonEmpty) {
      val t = worklist.remove(0)

      for (r <- pds.getNormalRulesEnding(t.getStart, t.getLabel)) {
        val previous = LinkedList(t)
        update(Transition(r.getS1, r.getL1, t.getTarget), r.getWeight, previous)
      }
      for (r <- pds.getPushRulesEnding(t.getStart, t.getLabel)) {
        for (tdash <- fa.getTransitions.toSet) {
          if (tdash.getLabel == r.getCallSite) {
            val previous = LinkedList(t, tdash)
            update(Transition(r.getS1, r.getL1, tdash.getTarget), r.getWeight, previous)
          } else if (r.getCallSite.isInstanceOf[Wildcard]) {
            val previous = LinkedList(t, tdash)
            update(Transition(r.getS1, tdash.getLabel, tdash.getTarget), r.getWeight, previous)
          }
        }
      }

      for (r <- pds.getPushRules) {
        if (!r.getCallSite.isInstanceOf[Wildcard] && r.getCallSite != t.getLabel) {
          continue
        }
        val tdash = Transition(r.getS2, r.getL2, t.getTarget)
        if (!fa.getTransitions.contains(tdash)) {
          continue
        }
        val previous = LinkedList(tdash, t)
        val label = if (r.getCallSite.isInstanceOf[Wildcard]) t.getLabel else r.getL1
        update(Transition(r.getS1, label, t.getTarget), r.getWeight, previous)
      }
    }

    fa
  }

  private def update(trans: Transition[N, D], weight: W, previous: List[Transition[N, D]]): Unit = {
    if (trans.getLabel.isInstanceOf[Wildcard]) throw new RuntimeException("INVALID TRANSITION")
    fa.addTransition(trans)
    var lt = getOrCreateWeight(trans)
    var fr = weight
    for (prev <- previous) {
      fr = fr.extendWith(getOrCreateWeight(prev)).asInstanceOf[W]
    }
    val newLt = lt.combineWith(fr).asInstanceOf[W]
    fa.addWeightForTransition(trans, newLt)
    if (!lt.equals(newLt)) {
      worklist += trans
    }
  }

  private def getOrCreateWeight(trans: Transition[N, D]): W = {
    val w = fa.getWeightFor(trans)
    if (w != null) return w

    // z.setRange(trans.getLabel(), trans.getLabel());
    null.asInstanceOf[W]
  }


}