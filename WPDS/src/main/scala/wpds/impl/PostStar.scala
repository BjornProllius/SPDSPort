/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */

package wpds.impl

import wpds.impl.Weight
import wpds.impl.NormalRule
import wpds.interfaces._
import wpds.wildcard._

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
    private var aut: WeightedPAutomaton[N, D, W] = fa

    override def onRuleAdded(rule: Rule[N, D, W]): Unit = {
      rule match {
        case _: NormalRule[N, D, W] => fa.registerListener(new HandleNormalListener(rule.asInstanceOf[NormalRule[N, D, W]]))
        case _: PushRule[N, D, W] => fa.registerListener(new HandlePushListener(rule.asInstanceOf[PushRule[N, D, W]]))
        case _: PopRule[N, D, W] => fa.registerListener(new HandlePopListener(rule.getS1, rule.getL1, rule.getS2, rule.getWeight))
        case _ =>
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (aut == null) 0 else aut.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: PostStarUpdateListener =>
          if (this == other) return true
          if (other == null) return false
          if (getClass != other.getClass) return false
          if (aut == null) {
            if (other.aut != null) return false
          } else if (!aut.equals(other.aut)) return false
          true
        case _ => false
      }
    }
  }

  private class UpdateTransitivePopListener(var start: D, protected var label: N, target: D, protected var newWeight: W)
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

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: UpdateTransitivePopListener =>
          if (this == other) return true
          if (!super.equals(other)) return false
          if (getClass != other.getClass) return false
          if (start == null) {
            if (other.start != null) return false
          } else if (!start.equals(other.start)) return false
          if (newWeight == null) {
            if (other.newWeight != null) return false
          } else if (!newWeight.equals(other.newWeight)) return false
          if (label == null) {
            if (other.label != null) return false
          } else if (!label.equals(other.label)) return false
          true
        case _ => false
      }
    }
  }

  private class HandlePopListener(state: D, val popLabel: N, val targetState: D, val ruleWeight: W) extends WPAStateListener[N, D, W](state) {

    override def onOutTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      if (t.getLabel.accepts(popLabel) || popLabel.accepts(t.getLabel)) {
        if (fa.isGeneratedState(t.getTarget)) {
          if (popLabel.isInstanceOf[Empty]) {
            throw new RuntimeException("IllegalState")
          }
          val newWeight = weight.extendWith(ruleWeight).asInstanceOf[W]
          update(new Transition(targetState, fa.epsilon, t.getTarget), newWeight)
          fa.registerListener(
            new UpdateTransitivePopListener(targetState, t.getLabel, t.getTarget, newWeight))
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
        fa.registerListener(
          new HandlePopListener(t.getTarget, popLabel, targetState, ruleWeight))
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

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: HandlePopListener =>
          if (this == other) return true
          if (!super.equals(other)) return false
          if (getClass != other.getClass) return false
          if (popLabel == null) {
            if (other.popLabel != null) return false
          } else if (!popLabel.equals(other.popLabel)) return false
          if (ruleWeight == null) {
            if (other.ruleWeight != null) return false
          } else if (!ruleWeight.equals(other.ruleWeight)) return false
          if (targetState == null) {
            if (other.targetState != null) return false
          } else if (!targetState.equals(other.targetState)) return false
          true
        case _ => false
      }
    }
  }

  private class HandleNormalListener(val rule: NormalRule[N, D, W]) extends WPAStateListener[N, D, W](rule.getS1) {

    override def onOutTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      if (t.getLabel == rule.getL1 || rule.getL1.isInstanceOf[Wildcard]) {
        var newWeight = weight.extendWith(rule.getWeight).asInstanceOf[W]
        val p = rule.getS2
        var l2 = rule.getL2
        l2 match {
          case ex: ExclusionWildcard[N] =>
            if (t.getLabel == ex.excludes()) return
          case _: Wildcard =>
            l2 = t.getLabel
            if (l2 == fa.epsilon) return
          case _ =>
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

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: HandleNormalListener =>
          if (this == other) return true
          if (!super.equals(other)) return false
          if (getClass != other.getClass) return false
          if (rule == null) {
            if (other.rule != null) return false
          } else if (!rule.equals(other.rule)) return false
          true
        case _ => false
      }
    }
  }

  private class HandlePushListener(_rule: PushRule[N, D, W]) extends WPAStateListener[N, D, W](_rule.getS1) {
    def rule = _rule
    override def onOutTransitionAdded(t: Transition[N, D], weight: W, aut: WeightedPAutomaton[N, D, W]): Unit = {
      if (t.getLabel == rule.getL1 || rule.getL1.isInstanceOf[Wildcard]) {
        if (rule.getCallSite.isInstanceOf[Wildcard]) {
          if (t.getLabel == fa.epsilon) return
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
              if (t.getLabel == fa.epsilon && t.getTarget == irState) {
                update(t, w.asInstanceOf[W])
                val newWeight = getWeightFor(callSiteTransition)
                update(new Transition(t.getStart, callSiteTransition.getLabel, callSiteTransition.getTarget),
                  newWeight.extendWith(w).asInstanceOf[W])
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

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: HandlePushListener =>
          if (this == other) return true
          if (!super.equals(other)) return false
          if (getClass != other.getClass) return false
          if (rule == null) {
            if (other.rule != null) return false
          } else if (!rule.equals(other.rule)) return false
          true
        case _ => false
      }
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