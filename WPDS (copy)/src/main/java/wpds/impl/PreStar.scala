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

import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import wpds.interfaces.IPushdownSystem
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.wildcard.Wildcard

class PreStar[N <: Location, D <: State, W <: Weight] {
  private var worklist = Lists.newLinkedList
  private var pds: Nothing = null
  private var fa: Nothing = null

  def prestar(pds: Nothing, initialAutomaton: Nothing): Nothing = {
    this.pds = pds
    worklist = Lists.newLinkedList(initialAutomaton.getTransitions)
    fa = initialAutomaton
    import scala.collection.JavaConversions._
    for (trans <- Sets.newHashSet(fa.getTransitions)) {
      val one = fa.getOne
      fa.addWeightForTransition(trans, one)
    }
    import scala.collection.JavaConversions._
    for (r <- pds.getPopRules) {
      update(new Nothing(r.getS1, r.getL1, r.getS2), r.getWeight, Lists.newLinkedList[Nothing])
    }
    while (!worklist.isEmpty) {
      val t = worklist.removeFirst
      import scala.collection.JavaConversions._
      for (r <- pds.getNormalRulesEnding(t.getStart, t.getLabel)) {
        // Normal rules
        val previous = Lists.newLinkedList[Nothing]
        previous.add(t)
        update(new Nothing(r.getS1, r.getL1, t.getTarget), r.getWeight, previous)
      }
      import scala.collection.JavaConversions._
      for (r <- pds.getPushRulesEnding(t.getStart, t.getLabel)) {
        // Push rules
        import scala.collection.JavaConversions._
        for (tdash <- Sets.newHashSet(fa.getTransitions)) {
          if (tdash.getLabel.equals(r.getCallSite)) {
            val previous = Lists.newLinkedList[Nothing]
            previous.add(t)
            previous.add(tdash)
            update(new Nothing(r.getS1, r.getL1, tdash.getTarget), r.getWeight, previous)
          }
          else if (r.getCallSite.isInstanceOf[Nothing]) {
            val previous = Lists.newLinkedList[Nothing]
            previous.add(t)
            previous.add(tdash)
            update(new Nothing(r.getS1, tdash.getLabel, tdash.getTarget), r.getWeight, previous)
          }
        }
      }
      import scala.collection.JavaConversions._
      for (r <- pds.getPushRules) {
        if (!r.getCallSite.isInstanceOf[Nothing] && !r.getCallSite.equals(t.getLabel)) continue //todo: continue is not supported
        val tdash = new Nothing(r.getS2, r.getL2, t.getTarget)
        if (!fa.getTransitions.contains(tdash)) continue //todo: continue is not supported
        val previous = Lists.newLinkedList[Nothing]
        previous.add(tdash)
        previous.add(t)
        val label = if ((r.getCallSite.isInstanceOf[Nothing])) t.getLabel
        else r.getL1
        update(new Nothing(r.getS1, label, t.getTarget), r.getWeight, previous)
      }
    }
    fa
  }

  private def update(trans: Nothing, weight: W, previous: Nothing): Unit = {
    if (trans.getLabel.isInstanceOf[Nothing]) throw new Nothing("INVALID TRANSITION")
    fa.addTransition(trans)
    val lt = getOrCreateWeight(trans)
    var fr = weight
    import scala.collection.JavaConversions._
    for (prev <- previous) {
      fr = fr.extendWith(getOrCreateWeight(prev)).asInstanceOf[W]
    }
    val newLt = lt.combineWith(fr).asInstanceOf[W]
    fa.addWeightForTransition(trans, newLt)
    if (!lt.equals(newLt)) worklist.add(trans)
  }

  private def getOrCreateWeight(trans: Nothing): W = {
    val w = fa.getWeightFor(trans)
    if (w != null) return w
    // z.setRange(trans.getLabel(), trans.getLabel());
    null
  }
}