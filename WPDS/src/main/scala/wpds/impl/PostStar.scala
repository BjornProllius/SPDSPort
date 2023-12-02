package wpds.impl

import wpds.interfaces.{Empty, IPushdownSystem, Location, State, WPAStateListener, WPAUpdateListener, WPDSUpdateListener}
import wpds.wildcard.{ExclusionWildcard, Wildcard}

abstract class PostStar[N <: Location, D <: State, W <: Weight] {
  private var pds: IPushdownSystem[N, D, W] = _
  private var fa: WeightedPAutomaton[N, D, W] = _

  def poststar(pds: IPushdownSystem[N, D, W], initialAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
    this.pds = pds
    this.fa = initialAutomaton
    fa.setInitialAutomaton(fa)
    this.pds.registerUpdateListener(new PostStarUpdateListener(fa))
  }

  private class PostStarUpdateListener(fa: WeightedPAutomaton[N, D, W]) extends WPDSUpdateListener[N, D, W] {
    private val aut: WeightedPAutomaton[N, D, W] = fa

    override def onRuleAdded(rule: Rule[N, D, W]): Unit = {
      rule match {
        case r: NormalRule[N, D, W] => fa.registerListener(new HandleNormalListener(r))
        case r: PushRule[N, D, W] => fa.registerListener(new HandlePushListener(r))
        case r: PopRule[N, D, W] => fa.registerListener(new HandlePopListener(r.getS1, r.getL1, r.getS2, r.getWeight))
        case _ =>
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (aut == null) 0 else aut.hashCode)
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: PostStarUpdateListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          aut == other.aut)
      case _ => false
    }
  }

  private class UpdateTransitivePopListener(start: D, label: N, target: D, newWeight: W) 
    extends WPAStateListener[N, D, W](target) {

    override def onOutTransitionAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      val extendWith = w.extendWith(newWeight).asInstanceOf[W]
      update(new Transition(start, t.getLabel, t.getTarget), extendWith)
    }

    override def onInTransitionAdded(t: Transition[N, D], w: W, aut: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (start == null) 0 else start.hashCode())
      result = prime * result + (if (newWeight == null) 0 else newWeight.hashCode())
      result = prime * result + (if (label == null) 0 else label.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: UpdateTransitivePopListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          start == other.start && newWeight == other.newWeight && label == other.label)
      case _ => false
    }
  }

  private class HandlePopListener(state: D, var popLabel: N, var targetState: D, var ruleWeight: W) 
    extends WPAStateListener[N, D, W](state) {

    override def onOutTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      if (t.getLabel.accepts(popLabel) || popLabel.accepts(t.getLabel)) {
        if (fa.isGeneratedState(t.getTarget)) {
          if (popLabel.isInstanceOf[Empty]) {
            throw new RuntimeException("IllegalState")
          }
          val newWeight = weight.extendWith(ruleWeight).asInstanceOf[W]
          update(new Transition(targetState, fa.epsilon(), t.getTarget), newWeight)
          fa.registerListener(new UpdateTransitivePopListener(targetState, t.getLabel, t.getTarget, newWeight))
          aut.registerSummaryEdge(t)
        } else if (fa.isUnbalancedState(t.getTarget)) {
          if (popLabel.isInstanceOf[Empty]) {
            throw new RuntimeException("IllegalState")
          }
          val newWeight = weight.extendWith(ruleWeight).asInstanceOf[W]
          fa.unbalancedPop(targetState, t, weight)
        }
      }
      if (t.getLabel.isInstanceOf[Empty]) {
        fa.registerListener(new HandlePopListener(t.getTarget, popLabel, targetState, ruleWeight))
      }
    }

    override def onInTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (popLabel == null) 0 else popLabel.hashCode())
      result = prime * result + (if (ruleWeight == null) 0 else ruleWeight.hashCode())
      result = prime * result + (if (targetState == null) 0 else targetState.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: HandlePopListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          popLabel == other.popLabel && ruleWeight == other.ruleWeight && targetState == other.targetState)
      case _ => false
    }
  }



  private class HandleNormalListener(rule: NormalRule[N, D, W]) 
    extends WPAStateListener[N, D, W](rule.getS1) {

    override def onOutTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      if (t.getLabel == rule.getL1 || rule.getL1.isInstanceOf[Wildcard]) {
        var newWeight = weight.extendWith(rule.getWeight).asInstanceOf[W]
        val p = rule.getS2
        var l2 = rule.getL2
        l2 match {
          case ex: ExclusionWildcard[N] =>
            if (t.getLabel == ex.excludes()) return
          case _ =>
        }
        if (l2.isInstanceOf[Wildcard]) {
          l2 = t.getLabel
          if (l2 == fa.epsilon()) return
        }
        if (!rule.canBeApplied(t, weight)) {
          return
        }
        update(new Transition(p, l2, t.getTarget), newWeight)
      }
    }

    override def onInTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (rule == null) 0 else rule.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: HandleNormalListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          rule == other.rule)
      case _ => false
    }
  }

  private class HandlePushListener(rule: PushRule[N, D, W]) 
    extends WPAStateListener[N, D, W](rule.getS1) {

    override def onOutTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      if (t.getLabel == rule.getL1 || rule.getL1.isInstanceOf[Wildcard]) {
        if (rule.getCallSite.isInstanceOf[Wildcard]) {
          if (t.getLabel == fa.epsilon()) return
        }
        val p = rule.getS2
        val gammaPrime = rule.getL2
        val irState = fa.createState(p, gammaPrime)
        val transitionLabel =
          if (rule.getCallSite.isInstanceOf[Wildcard]) t.getLabel else rule.getCallSite
        val callSiteTransition = new Transition(irState, transitionLabel, t.getTarget)
        val calleeTransition = new Transition(p, gammaPrime, irState)
        val weightAtCallsite = weight.extendWith(rule.getWeight).asInstanceOf[W]
        update(callSiteTransition, weightAtCallsite)
        if (!fa.nested()) {
          update(calleeTransition, fa.getOne)
        } else {
          if (!fa.isGeneratedState(irState)) throw new RuntimeException("State must be generated")
          val summary = getOrCreateSummaryAutomaton(irState, calleeTransition, fa.getOne, aut)
          summary.registerListener(new WPAUpdateListener[N, D, W] {
            override def onWeightAdded(t: Transition[N, D], w: W, innerAut: WeightedPAutomaton[N, D, W]): Unit = {
              if (t.getLabel == fa.epsilon() && t.getTarget == irState) {
                update(t, w)
                val newWeight = getWeightFor(callSiteTransition)
                update(new Transition(t.getStart, callSiteTransition.getLabel, callSiteTransition.getTarget), newWeight.extendWith(w))
              }
            }
          })
        }
      }
    }

    override def onInTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (rule == null) 0 else rule.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: HandlePushListener =>
        (this eq other) || (other != null && getClass == other.getClass &&
          rule == other.rule)
      case _ => false
    }
  }

  private def update(trans: Transition[N, D], weight: W): Unit = {
    if (!fa.nested()) {
      fa.addWeightForTransition(trans, weight)
    } else {
      getSummaryAutomaton(trans.getTarget).addWeightForTransition(trans, weight)
    }
  }

  private def getWeightFor(trans: Transition[N, D]): W = {
    if (!fa.nested()) {
      fa.getWeightFor(trans)
    } else {
      getSummaryAutomaton(trans.getTarget).getWeightFor(trans)
    }
  }

  private def getOrCreateSummaryAutomaton(
      target: D, transition: Transition[N, D], weight: W, context: WeightedPAutomaton[N, D, W]): WeightedPAutomaton[N, D, W] = {
    var aut = getSummaryAutomaton(target)
    if (aut == null) {
      aut = context.createNestedAutomaton(target)
      putSummaryAutomaton(target, aut)
      aut.setInitialAutomaton(fa)
    } else {
      context.addNestedAutomaton(aut)
    }
    aut.addWeightForTransition(transition, weight)
    aut
  }

  def putSummaryAutomaton(target: D, aut: WeightedPAutomaton[N, D, W]): Unit

  def getSummaryAutomaton(target: D): WeightedPAutomaton[N, D, W]

}