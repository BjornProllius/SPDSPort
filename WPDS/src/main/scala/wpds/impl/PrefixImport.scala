package wpds.impl

import wpds.interfaces.{Location, State, WPAStateListener}

import wpds.interfaces.{Location, State, Weight}

class PrefixImport[N <: Location, D <: State, W <: Weight](autA: WeightedPAutomaton[N, D, W], autB: WeightedPAutomaton[N, D, W], t: Transition[N, D]) {

  private val baseAutomaton: WeightedPAutomaton[N, D, W] = autA
  private val flowAutomaton: WeightedPAutomaton[N, D, W] = autB

  abstract class IntersectionCallback {
    def trigger(baseT: Transition[N, D], flowT: Transition[N, D]): Unit
  }

  baseAutomaton.registerListener(
    new IntersectionListener(
      t.getStart,
      t.getStart,
      t.getLabel,
      new IntersectionCallback {
        override def trigger(baseT: Transition[N, D], flowT: Transition[N, D]): Unit = {
          baseAutomaton.registerListener(new Import(t.getTarget, flowT.getTarget))
          baseAutomaton.registerListener(
            new IntersectionListenerNoLabel(
              t.getTarget,
              flowT.getTarget,
              new IntersectionCallback {
                override def trigger(baseT: Transition[N, D], flowT: Transition[N, D]): Unit = {
                  baseAutomaton.registerListener(new Import(baseT.getTarget, flowT.getTarget))
                }
              }
            )
          )
        }
      }
    )
  )

  private class Import(state: D, var flowTarget: D) extends WPAStateListener[N, D, W](state) {

    override def onOutTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {}

    override def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
      flowAutomaton.addTransition(new Transition[N, D](t.getStart, t.getLabel, flowTarget))
      baseAutomaton.registerListener(new Import(t.getStart, t.getStart))
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (flowTarget == null) 0 else flowTarget.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: Import =>
        (this eq other) || (other != null && getClass == other.getClass &&
          flowTarget == other.flowTarget)
      case _ => false
    }
  }
  private class IntersectionListener(baseState: D, var flowState: D, var label: N, var callback: IntersectionCallback) 
    extends WPAStateListener[N, D, W](baseState) {

    override def onOutTransitionAdded(baseT: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
      if (!baseT.getLabel.equals(label)) return
      flowAutomaton.registerListener(new HasOutTransWithSameLabel(flowState, baseT, callback))
    }

    override def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (flowState == null) 0 else flowState.hashCode())
      result = prime * result + (if (label == null) 0 else label.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: IntersectionListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          flowState == other.flowState && label == other.label)
      case _ => false
    }
  }
  protected class HasOutTransWithSameLabel(state: D, baseT: Transition[N, D], callback: IntersectionCallback) 
    extends WPAStateListener[N, D, W](state) {

    override def onOutTransitionAdded(flowT: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
      if (flowT.getLabel.equals(baseT.getLabel)) {
        callback.trigger(baseT, flowT)
      }
    }

    override def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (baseT == null) 0 else baseT.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: HasOutTransWithSameLabel =>
        (this eq other) || (other != null && getClass == other.getClass &&
          baseT == other.baseT)
      case _ => false
    }
  }

  private class IntersectionListenerNoLabel(baseState: D, var flowState: D, var callback: IntersectionCallback) 
    extends WPAStateListener[N, D, W](baseState) {

    override def onOutTransitionAdded(baseT: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
      flowAutomaton.registerListener(new HasOutTransWithSameLabel(flowState, baseT, callback))
    }

    override def onInTransitionAdded(t: Transition[N, D], w: W, weightedPAutomaton: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (flowState == null) 0 else flowState.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: IntersectionListenerNoLabel =>
        (this eq other) || (other != null && getClass == other.getClass &&
          flowState == other.flowState)
      case _ => false
    }
  }



}