package wpds.impl

import com.google.common.collect.{Lists, Sets}
import scala.collection.mutable.Queue
import wpds.interfaces.{IPushdownSystem, Location, State}
import wpds.wildcard.Wildcard
import scala.jdk.CollectionConverters._

class PreStar[N <: Location, D <: State, W <: Weight] {
  private var worklist: Queue[Transition[N, D]] = Queue()
  private var pds: IPushdownSystem[N, D, W] = _
  private var fa: WeightedPAutomaton[N, D, W] = _

  def prestar(pds: IPushdownSystem[N, D, W], initialAutomaton: WeightedPAutomaton[N, D, W]): WeightedPAutomaton[N, D, W] = {
    this.pds = pds
    worklist = Queue(initialAutomaton.getTransitions.asScala.toSeq:_*)
    fa = initialAutomaton

    for (trans <- Sets.newHashSet(fa.getTransitions).asScala) {
      val one = fa.getOne
      fa.addWeightForTransition(trans, one)
    }
    for (r <- pds.getPopRules) {
      update(
        new Transition[N, D](r.getS1, r.getL1, r.getS2),
        r.getWeight,
        List[Transition[N, D]]()
      )
    }

    while (worklist.nonEmpty) {
      val t = worklist.dequeue()

      for (r <- pds.getNormalRulesEnding(t.getStart(), t.getLabel())) {
        // Normal rules
        val previous = List(t)
        update(new Transition[N, D](r.getS1, r.getL1, t.getTarget()), r.getWeight, previous)
      }
      for (r <- pds.getPushRulesEnding(t.getStart(), t.getLabel())) {
        // Push rules
        for (tdash <- Sets.newHashSet(fa.getTransitions)) {
          if (tdash.getLabel().equals(r.getCallSite())) {
            val previous = List(t, tdash)
            update(
              new Transition[N, D](r.getS1(), r.getL1(), tdash.getTarget()),
              r.getWeight(),
              previous)
          } else if (r.getCallSite().isInstanceOf[Wildcard]) {
            val previous = List(t, tdash)
            update(
              new Transition[N, D](r.getS1(), tdash.getLabel(), tdash.getTarget()),
              r.getWeight(),
              previous)
          }
        }
      }

      for (r <- pds.getPushRules) {
        if (r.getCallSite.isInstanceOf[Wildcard] || r.getCallSite.equals(t.getLabel)) {
          val tdash = new Transition[N, D](r.getS2, r.getL2, t.getTarget)
          if (fa.getTransitions.contains(tdash)) {
            val previous = List[Transition[N, D]](tdash, t)
            val label = if (r.getCallSite.isInstanceOf[Wildcard]) t.getLabel else r.getL1
            update(new Transition[N, D](r.getS1, label, t.getTarget), r.getWeight, previous)
          }
        }
      }
    }
    fa
  }

  private def update(trans: Transition[N, D], weight: W, previous: List[Transition[N, D]]): Unit = {
    if (trans.getLabel().isInstanceOf[Wildcard])
      throw new RuntimeException("INVALID TRANSITION")
    fa.addTransition(trans)
    var lt = getOrCreateWeight(trans)
    var fr = weight
    for (prev <- previous) {
      fr = fr.extendWith(getOrCreateWeight(prev)).asInstanceOf[W]
    }
    val newLt = lt.combineWith(fr).asInstanceOf[W]
    fa.addWeightForTransition(trans, newLt)
    if (!lt.equals(newLt)) {
      worklist.enqueue(trans)
    }
  }

  private def getOrCreateWeight(trans: Transition[N, D]): W = {
    val w = fa.getWeightFor(trans)
    if (w != null)
      return w

    // z.setRange(trans.getLabel(), trans.getLabel());
    null
  }
}