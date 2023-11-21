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
package tests

import tests.TestHelper.a
import tests.TestHelper.accepts
import tests.TestHelper.normal
import tests.TestHelper.pop
import tests.TestHelper.push
import tests.TestHelper.s
import tests.TestHelper.t
import com.google.common.base.Joiner
import com.google.common.collect.Sets
import java.util
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import tests.TestHelper.Abstraction
import tests.TestHelper.StackSymbol
import wpds.impl.PAutomaton
import wpds.impl.PushdownSystem
import wpds.impl.SummaryNestedWeightedPAutomatons
import wpds.impl.Transition
import wpds.impl.Weight.NoWeight
import wpds.interfaces.ReachabilityListener

class ForwardDFSVisitorTest {
  private[tests] val fa = new Nothing() {
    @Override def createState(d: Nothing, loc: Nothing) = new Nothing(d, loc)

    @Override def epsilon: Nothing = s("EPS")

    @Override def isGeneratedState(d: Nothing): Boolean = d.s != null
  }
  final private[tests] val reachables = Sets.newHashSet

  @Test def delayedAdd(): Unit = {
    fa.registerDFSListener(a(0), new Nothing() {
      @Override def reachable(t: Nothing): Unit = {
        reachables.add(t)
      }
    })
    fa.addTransition(t(0, "n1", 1))
    Assert.assertFalse(reachables.isEmpty)
    Assert.assertTrue(reachableMinusTrans.isEmpty)
    fa.addTransition(t(1, "n1", 2))
    Assert.assertTrue(reachableMinusTrans.isEmpty)
  }

  @Test def delayedAddListener(): Unit = {
    fa.addTransition(t(0, "n1", 1))
    fa.addTransition(t(1, "n1", 2))
    Assert.assertFalse(fa.getTransitions.isEmpty)
    fa.registerDFSListener(a(0), new Nothing() {
      @Override def reachable(t: Nothing): Unit = {
        reachables.add(t)
      }
    })
    Assert.assertFalse(reachables.isEmpty)
    Assert.assertTrue(reachableMinusTrans.isEmpty)
    fa.addTransition(t(4, "n1", 5))
    Assert.assertTrue(fa.getTransitions.size > reachables.size)
    fa.addTransition(t(2, "n1", 5))
    Assert.assertTrue(fa.getTransitions.size > reachables.size)
    Assert.assertFalse(reachableMinusTrans.isEmpty)
    fa.addTransition(t(2, "n1", 4))
    Assert.assertTrue(fa.getTransitions.size eq reachables.size)
    Assert.assertTrue(reachableMinusTrans.isEmpty)
    fa.addTransition(t(3, "n1", 8))
    fa.addTransition(t(8, "n1", 9))
    fa.addTransition(t(3, "n1", 7))
    fa.addTransition(t(3, "n1", 6))
    fa.addTransition(t(6, "n1", 3))
    Assert.assertTrue(fa.getTransitions.size > reachables.size)
    Assert.assertFalse(reachableMinusTrans.isEmpty)
    fa.addTransition(t(1, "n1", 3))
    Assert.assertTrue(reachableMinusTrans.isEmpty)
  }

  private var pds: Nothing = null

  @Before def init(): Unit = {
    pds = new Nothing() {}
  }

  @Test def summaryReachabilityTest(): Unit = {
    pds.addRule(normal(1, "a", 1, "b"))
    pds.addRule(normal(1, "b", 1, "c"))
    pds.addRule(push(1, "c", 1, "d", "e"))
    pds.addRule(push(1, "d", 1, "h", "i"))
    pds.addRule(normal(1, "h", 2, "g"))
    pds.addRule(pop(2, "g", 1))
    pds.addRule(pop(1, "d", 1))
    pds.addRule(normal(1, "e", 1, "k"))
    val fa = accepts(1, "a")
    pds.poststar(fa)
    val faSummaries = accepts(1, "a")
    pds.poststar(faSummaries, new Nothing)
    assertSetEquals(reachableFrom(fa, a(2)), reachableFrom(faSummaries, a(2)))
    assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
  }

  @Test def simpleSummaryReachabilityTest(): Unit = {
    pds.addRule(normal(1, "a", 2, "b"))
    pds.addRule(push(2, "b", 3, "d", "e"))
    pds.addRule(normal(3, "d", 3, "f"))
    pds.addRule(pop(3, "f", 2))
    pds.addRule(normal(2, "e", 3, "k"))
    val fa = accepts(1, "a")
    pds.poststar(fa)
    val faSummaries = accepts(1, "a")
    pds.poststar(faSummaries, new Nothing)
    assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
  }

  @Test def reapplySummaryReachabilityTest(): Unit = {
    pds.addRule(normal(1, "a", 2, "b"))
    pds.addRule(push(2, "b", 3, "d", "e"))
    pds.addRule(normal(3, "d", 3, "f"))
    pds.addRule(normal(3, "f", 4, "j"))
    pds.addRule(pop(4, "j", 2))
    pds.addRule(normal(2, "e", 4, "k"))
    pds.addRule(push(4, "k", 3, "d", "i"))
    pds.addRule(normal(2, "i", 5, "m"))
    val fa = accepts(1, "a")
    pds.poststar(fa)
    val faSummaries = accepts(1, "a")
    pds.poststar(faSummaries, new Nothing)
    assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
  }

  @Test def doublePushSummaryReachabilityTest(): Unit = {
    pds.addRule(normal(1, "a", 2, "b"))
    pds.addRule(push(2, "b", 3, "d", "e"))
    pds.addRule(normal(3, "d", 3, "f"))
    pds.addRule(push(3, "f", 4, "l", "k"))
    pds.addRule(normal(4, "l", 5, "m"))
    pds.addRule(pop(5, "m", 4))
    pds.addRule(normal(4, "k", 3, "z"))
    pds.addRule(pop(3, "z", 2))
    pds.addRule(normal(2, "e", 6, "i"))
    val fa = accepts(1, "a")
    pds.poststar(fa)
    val faSummaries = accepts(1, "a")
    pds.poststar(faSummaries, new Nothing)
    assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
  }

  private def assertSetEquals(s1: Nothing, s2: Nothing): Unit = {
    if (s1.equals(s2)) return
    val s1MinusS2 = Sets.newHashSet(s1)
    s1MinusS2.removeAll(s2)
    val s2MinusS1 = Sets.newHashSet(s2)
    s2MinusS1.removeAll(s1)
    throw new Nothing("The sets are not equal: \n S1\\S2 = \n" + Joiner.on("\n\t").join(s1MinusS2) + " \n S2\\S1 = \n" + Joiner.on("\n\t").join(s2MinusS1))
  }

  private def reachableFrom(aut: Nothing, a: Nothing) = {
    val reachable = Sets.newHashSet
    aut.registerDFSListener(a, new Nothing() {
      @Override def reachable(t: Nothing): Unit = {
        reachable.add(t)
      }
    })
    reachable
  }

  private def reachableMinusTrans = {
    val res = Sets.newHashSet(fa.getTransitions)
    res.removeAll(reachables)
    res
  }
}