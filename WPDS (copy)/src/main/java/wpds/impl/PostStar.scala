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

import wpds.interfaces.Empty
import wpds.interfaces.IPushdownSystem
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.WPAStateListener
import wpds.interfaces.WPAUpdateListener
import wpds.interfaces.WPDSUpdateListener
import wpds.wildcard.ExclusionWildcard
import wpds.wildcard.Wildcard

abstract class PostStar[N <: Location, D <: State, W <: Weight] {
  private var pds: Nothing = null
  private var fa: Nothing = null

  def poststar(pds: Nothing, initialAutomaton: Nothing): Unit = {
    this.pds = pds
    this.fa = initialAutomaton
    fa.setInitialAutomaton(fa)
    this.pds.registerUpdateListener(new PostStar[N, D, W]#PostStarUpdateListener(fa))
  }

  private class PostStarUpdateListener(private var aut: Nothing) extends Nothing {
    @Override def onRuleAdded(rule: Nothing): Unit = {
      if (rule.isInstanceOf[Nothing]) fa.registerListener(new PostStar[N, D, W]#HandleNormalListener(rule.asInstanceOf[Nothing]))
      else if (rule.isInstanceOf[Nothing]) fa.registerListener(new PostStar[N, D, W]#HandlePushListener(rule.asInstanceOf[Nothing]))
      else if (rule.isInstanceOf[Nothing]) fa.registerListener(new PostStar[N, D, W]#HandlePopListener(rule.getS1, rule.getL1, rule.getS2, rule.getWeight))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (aut == null) 0
      else aut.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PostStar[N, D, W]#PostStarUpdateListener]
      if (aut == null) if (other.aut != null) return false
      else if (!aut.equals(other.aut)) return false
      true
    }
  }

  private class UpdateTransitivePopListener(private var start: D, private var label: N, target: D, private var newWeight: W) extends Nothing(target) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, aut: Nothing): Unit = {
      val extendWith = w.extendWith(newWeight).asInstanceOf[W]
      update(new Nothing(start, t.getLabel, t.getTarget), extendWith)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, aut: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (start == null) 0
      else start.hashCode)
      result = prime * result + (if (newWeight == null) 0
      else newWeight.hashCode)
      result = prime * result + (if (label == null) 0
      else label.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PostStar[N, D, W]#UpdateTransitivePopListener]
      if (start == null) if (other.start != null) return false
      else if (!start.equals(other.start)) return false
      if (newWeight == null) if (other.newWeight != null) return false
      else if (!newWeight.equals(other.newWeight)) return false
      if (label == null) if (other.label != null) return false
      else if (!label.equals(other.label)) return false
      true
    }
  }

  private class HandlePopListener(state: D, private var popLabel: N, private var targetState: D, private var ruleWeight: W) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, weight: W, aut: Nothing): Unit = {
      if (t.getLabel.accepts(popLabel) || popLabel.accepts(t.getLabel)) if (fa.isGeneratedState(t.getTarget)) {
        if (popLabel.isInstanceOf[Nothing]) throw new Nothing("IllegalState")
        val newWeight = weight.extendWith(ruleWeight).asInstanceOf[W]
        update(new Nothing(targetState, fa.epsilon, t.getTarget), newWeight)
        fa.registerListener(new PostStar[N, D, W]#UpdateTransitivePopListener(targetState, t.getLabel, t.getTarget, newWeight))
        aut.registerSummaryEdge(t)
      }
      else if (fa.isUnbalancedState(t.getTarget)) {
        if (popLabel.isInstanceOf[Nothing]) throw new Nothing("IllegalState")
        val newWeight = weight.extendWith(ruleWeight).asInstanceOf[W]
        //                    fa.registerListener(new UpdateTransitivePopListener(
        //                       targetState, t.getTarget(), newWeight));
        fa.unbalancedPop(targetState, t, weight)
      }
      if (t.getLabel.isInstanceOf[Nothing]) fa.registerListener(new PostStar[N, D, W]#HandlePopListener(t.getTarget, popLabel, targetState, ruleWeight))
    }

    @Override def onInTransitionAdded(t: Nothing, weight: W, aut: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (popLabel == null) 0
      else popLabel.hashCode)
      result = prime * result + (if (ruleWeight == null) 0
      else ruleWeight.hashCode)
      result = prime * result + (if (targetState == null) 0
      else targetState.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PostStar[N, D, W]#HandlePopListener]
      if (popLabel == null) if (other.popLabel != null) return false
      else if (!popLabel.equals(other.popLabel)) return false
      if (ruleWeight == null) if (other.ruleWeight != null) return false
      else if (!ruleWeight.equals(other.ruleWeight)) return false
      if (targetState == null) if (other.targetState != null) return false
      else if (!targetState.equals(other.targetState)) return false
      true
    }
  }

  private class HandleNormalListener(private var rule: Nothing) extends Nothing(rule.getS1) {
    @Override def onOutTransitionAdded(t: Nothing, weight: W, aut: Nothing): Unit = {
      if (t.getLabel.equals(rule.getL1) || rule.getL1.isInstanceOf[Nothing]) {
        val newWeight = weight.extendWith(rule.getWeight).asInstanceOf[W]
        val p = rule.getS2
        var l2 = rule.getL2
        if (l2.isInstanceOf[Nothing]) {
          val ex = l2.asInstanceOf[Nothing]
          if (t.getLabel.equals(ex.excludes)) return
        }
        if (l2.isInstanceOf[Nothing]) {
          l2 = t.getLabel
          if (l2.equals(fa.epsilon)) return
        }
        if (!rule.canBeApplied(t, weight)) return
        update(new Nothing(p, l2, t.getTarget), newWeight)
      }
    }

    @Override def onInTransitionAdded(t: Nothing, weight: W, aut: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (rule == null) 0
      else rule.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PostStar[N, D, W]#HandleNormalListener]
      if (rule == null) if (other.rule != null) return false
      else if (!rule.equals(other.rule)) return false
      true
    }
  }

  private class HandlePushListener(private var rule: Nothing) extends Nothing(rule.getS1) {
    @Override def onOutTransitionAdded(t: Nothing, weight: W, aut: Nothing): Unit = {
      if (t.getLabel.equals(rule.getL1) || rule.getL1.isInstanceOf[Nothing]) {
        if (rule.getCallSite.isInstanceOf[Nothing]) if (t.getLabel.equals(fa.epsilon)) return
        val p = rule.getS2
        val gammaPrime = rule.getL2
        val irState = fa.createState(p, gammaPrime)
        val transitionLabel = if (rule.getCallSite.isInstanceOf[Nothing]) t.getLabel
        else rule.getCallSite
        val callSiteTransition = new Nothing(irState, transitionLabel, t.getTarget)
        val calleeTransition = new Nothing(p, gammaPrime, irState)
        val weightAtCallsite = weight.extendWith(rule.getWeight).asInstanceOf[W]
        update(callSiteTransition, weightAtCallsite)
        if (!fa.nested) update(calleeTransition, fa.getOne)
        else {
          if (!fa.isGeneratedState(irState)) throw new Nothing("State must be generated")
          val summary = getOrCreateSummaryAutomaton(irState, calleeTransition, fa.getOne, aut)
          summary.registerListener(new Nothing() {
            @Override def onWeightAdded(t: Nothing, w: W, innerAut: Nothing): Unit = {
              if (t.getLabel.equals(fa.epsilon) && t.getTarget.equals(irState)) {
                update(t, w.asInstanceOf[W])
                val newWeight = getWeightFor(callSiteTransition)
                update(new Nothing(t.getStart, callSiteTransition.getLabel, callSiteTransition.getTarget), newWeight.extendWith(w).asInstanceOf[W])
              }
            }
          })
        }
      }
    }

    @Override def onInTransitionAdded(t: Nothing, weight: W, aut: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (rule == null) 0
      else rule.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PostStar[N, D, W]#HandlePushListener]
      if (rule == null) if (other.rule != null) return false
      else if (!rule.equals(other.rule)) return false
      true
    }
  }

  private def update(trans: Nothing, weight: W): Unit = {
    if (!fa.nested) fa.addWeightForTransition(trans, weight)
    else getSummaryAutomaton(trans.getTarget).addWeightForTransition(trans, weight)
  }

  private def getWeightFor(trans: Nothing) = if (!fa.nested) fa.getWeightFor(trans)
  else getSummaryAutomaton(trans.getTarget).getWeightFor(trans)

  private def getOrCreateSummaryAutomaton(target: D, transition: Nothing, weight: W, context: Nothing) = {
    var aut = getSummaryAutomaton(target)
    if (aut == null) {
      aut = context.createNestedAutomaton(target)
      putSummaryAutomaton(target, aut)
      aut.setInitialAutomaton(fa)
    }
    else context.addNestedAutomaton(aut)
    aut.addWeightForTransition(transition, weight)
    aut
  }

  def putSummaryAutomaton(target: D, aut: Nothing): Unit

  def getSummaryAutomaton(target: D): Nothing
}