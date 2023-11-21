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

import com.google.common.base.Joiner
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import wpds.interfaces.IPushdownSystem
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.WPDSUpdateListener
import wpds.wildcard.Wildcard

class WeightedPushdownSystem[N <: Location, D <: State, W <: Weight] extends Nothing {
  final protected val pushRules = Sets.newHashSet
  final protected val popRules = Sets.newHashSet
  final protected val normalRules = Sets.newHashSet
  final protected val listeners = Sets.newHashSet

  @Override def addRule(rule: Nothing): Boolean = {
    if (addRuleInternal(rule)) {
      import scala.collection.JavaConversions._
      for (l <- Lists.newArrayList(listeners)) {
        l.onRuleAdded(rule)
      }
      return true
    }
    false
  }

  private def addRuleInternal(rule: Nothing): Boolean = {
    if (rule.isInstanceOf[Nothing]) return pushRules.add(rule.asInstanceOf[Nothing])
    else if (rule.isInstanceOf[Nothing]) return popRules.add(rule.asInstanceOf[Nothing])
    else if (rule.isInstanceOf[Nothing]) return normalRules.add(rule.asInstanceOf[Nothing])
    throw new Nothing("Try to add a rule of wrong type")
  }

  def registerUpdateListener(listener: Nothing): Unit = {
    if (!listeners.add(listener)) return
    import scala.collection.JavaConversions._
    for (r <- getAllRules) {
      listener.onRuleAdded(r)
    }
  }

  @Override def getNormalRules: Nothing = normalRules

  @Override def getPopRules: Nothing = popRules

  @Override def getPushRules: Nothing = pushRules

  @Override def getAllRules: Nothing = {
    val rules = Sets.newHashSet
    rules.addAll(normalRules)
    rules.addAll(popRules)
    rules.addAll(pushRules)
    rules
  }

  @Override def getRulesStarting(start: D, string: N): Nothing = {
    val result = new Nothing
    getRulesStartingWithinSet(start, string, popRules, result)
    getRulesStartingWithinSet(start, string, normalRules, result)
    getRulesStartingWithinSet(start, string, pushRules, result)
    result
  }

  private def getRulesStartingWithinSet(start: D, string: N, rules: Nothing, res: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (r <- rules) {
      if (r.getS1.equals(start) && (r.getL1.equals(string) || r.getL1.isInstanceOf[Nothing])) res.add(r)
      if (string.isInstanceOf[Nothing] && r.getS1.equals(start)) res.add(r)
    }
  }

  @Override def getNormalRulesEnding(start: D, string: N): Nothing = {
    val allRules = getNormalRules
    val result = new Nothing
    import scala.collection.JavaConversions._
    for (r <- allRules) {
      if (r.getS2.equals(start) && r.getL2.equals(string)) result.add(r)
    }
    result
  }

  @Override def getPushRulesEnding(start: D, string: N): Nothing = {
    val allRules = getPushRules
    val result = new Nothing
    import scala.collection.JavaConversions._
    for (r <- allRules) {
      if (r.getS2.equals(start) && r.getL2.equals(string)) result.add(r)
    }
    result
  }

  @Override def getStates: Nothing = {
    val states = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (r <- getAllRules) {
      states.add(r.getS1)
      states.add(r.getS2)
    }
    states
  }

  @Override def poststar(initialAutomaton: Nothing, summaries: Nothing): Unit = {
    new Nothing() {
      @Override def putSummaryAutomaton(target: D, aut: Nothing): Unit = {
        summaries.putSummaryAutomaton(target, aut)
      }

      @Override def getSummaryAutomaton(target: D): Nothing = summaries.getSummaryAutomaton(target)
    }.poststar(this, initialAutomaton)
  }

  @Override def poststar(initialAutomaton: Nothing): Unit = {
    new Nothing() {
      @Override def putSummaryAutomaton(target: D, aut: Nothing): Unit = {
      }

      @Override def getSummaryAutomaton(target: D): Nothing = initialAutomaton
    }.poststar(this, initialAutomaton)
  }

  @Override def prestar(initialAutomaton: Nothing): Unit = {
    new Nothing().prestar(this, initialAutomaton)
  }

  def toString: Nothing = {
    var s = "WPDS (#Rules: " + getAllRules.size + ")\n"
    s += "\tNormalRules:\n\t\t"
    s += Joiner.on("\n\t\t").join(normalRules)
    s += "\n"
    s += "\tPopRules:\n\t\t"
    s += Joiner.on("\n\t\t").join(popRules)
    s += "\n"
    s += "\tPushRules:\n\t\t"
    s += Joiner.on("\n\t\t").join(pushRules)
    s
  }

  // @Override
  // public int hashCode() {
  // final int prime = 31;
  // int result = 1;
  // result = prime * result + ((normalRules == null) ? 0 : normalRules.hashCode());
  // result = prime * result + ((popRules == null) ? 0 : popRules.hashCode());
  // result = prime * result + ((pushRules == null) ? 0 : pushRules.hashCode());
  // return result;
  // }
  //
  // @Override
  // public boolean equals(Object obj) {
  // if (this == obj)
  // return true;
  // if (obj == null)
  // return false;
  // if (getClass() != obj.getClass())
  // return false;
  // PushdownSystem other = (PushdownSystem) obj;
  // if (normalRules == null) {
  // if (other.normalRules != null)
  // return false;
  // } else if (!normalRules.equals(other.normalRules))
  // return false;
  // if (popRules == null) {
  // if (other.popRules != null)
  // return false;
  // } else if (!popRules.equals(other.popRules))
  // return false;
  // if (pushRules == null) {
  // if (other.pushRules != null)
  // return false;
  // } else if (!pushRules.equals(other.pushRules))
  // return false;
  // return true;
  // }
  @Override def unregisterAllListeners(): Unit = {
    listeners.clear
  }
}