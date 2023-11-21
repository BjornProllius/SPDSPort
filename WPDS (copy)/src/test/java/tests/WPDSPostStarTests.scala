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

import org.junit.Assert.assertEquals
import tests.TestHelper.ACC
import tests.TestHelper.a
import tests.TestHelper.t
import tests.TestHelper.waccepts
import tests.TestHelper.wnormal
import tests.TestHelper.wpop
import tests.TestHelper.wpush
import java.util
import org.junit.Before
import org.junit.Test
import tests.TestHelper.Abstraction
import tests.TestHelper.StackSymbol
import wpds.impl.Transition
import wpds.impl.WeightedPAutomaton
import wpds.impl.WeightedPushdownSystem

object WPDSPostStarTests {
  private def w(i: Int) = new Nothing(i)
}

class WPDSPostStarTests {
  private var pds: Nothing = null

  @Before def init(): Unit = {
    pds = new Nothing
  }

  @Test def simple(): Unit = {
    pds.addRule(wnormal(1, "a", 2, "b", WPDSPostStarTests.w(2)))
    pds.addRule(wnormal(2, "b", 3, "c", WPDSPostStarTests.w(3)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(fa.getTransitions.size, 3)
    assertEquals(fa.getStates.size, 4)
    assertEquals(fa.getWeightFor(t(3, "c", ACC)), WPDSPostStarTests.w(5))
  }

  @Test def branch(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPostStarTests.w(2)))
    pds.addRule(wnormal(1, "b", 1, "c", WPDSPostStarTests.w(3)))
    pds.addRule(wnormal(1, "a", 1, "d", WPDSPostStarTests.w(3)))
    pds.addRule(wnormal(1, "d", 1, "c", WPDSPostStarTests.w(3)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(fa.getWeightFor(t(1, "c", ACC)), NumWeight.zero)
    assertEquals(fa.getWeightFor(t(1, "b", ACC)), WPDSPostStarTests.w(2))
    assertEquals(fa.getWeightFor(t(1, "d", ACC)), WPDSPostStarTests.w(3))
  }

  @Test def push1(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPostStarTests.w(2)))
    pds.addRule(wpush(1, "b", 1, "c", "d", WPDSPostStarTests.w(3)))
    pds.addRule(wnormal(1, "c", 1, "e", WPDSPostStarTests.w(1)))
    pds.addRule(wpop(1, "e", 1, WPDSPostStarTests.w(5)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(fa.getWeightFor(t(1, "b", ACC)), WPDSPostStarTests.w(2))
    assertEquals(fa.getWeightFor(t(1, "d", ACC)), WPDSPostStarTests.w(11))
    assertEquals(fa.getWeightFor(t(1, "e", a(1, "c"))), WPDSPostStarTests.w(1))
    val weights = fa.getTransitionsToFinalWeights
    //        assertEquals(weights.get(t(1, "e", a(1, "c"))), w(6));
  }

  @Test def push2(): Unit = {
    pds.addRule(wnormal(1, "a", 2, "b", WPDSPostStarTests.w(2)))
    pds.addRule(wpush(2, "b", 3, "c", "d", WPDSPostStarTests.w(3)))
    pds.addRule(wnormal(3, "c", 4, "e", WPDSPostStarTests.w(1)))
    pds.addRule(wpop(4, "e", 5, WPDSPostStarTests.w(5)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(fa.getWeightFor(t(5, "d", ACC)), WPDSPostStarTests.w(11))
  }

  @Test def twoCall(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPostStarTests.w(1)))
    pds.addRule(wpush(1, "b", 2, "call", "d", WPDSPostStarTests.w(2)))
    pds.addRule(wnormal(2, "call", 2, "e", WPDSPostStarTests.w(3)))
    pds.addRule(wpop(2, "e", 3, WPDSPostStarTests.w(4)))
    pds.addRule(wnormal(3, "d", 1, "f", WPDSPostStarTests.w(5)))
    pds.addRule(wpush(1, "f", 2, "call", "g", WPDSPostStarTests.w(6)))
    pds.addRule(wnormal(3, "g", 4, "h", WPDSPostStarTests.w(7)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(WPDSPostStarTests.w(15), fa.getWeightFor(t(1, "f", ACC)))
    //        assertEquals(w(7), fa.getWeightFor(t(3, "EPS", a(2, "call"))));
    assertEquals(WPDSPostStarTests.w(10), fa.getWeightFor(t(3, "d", ACC)))
    assertEquals(WPDSPostStarTests.w(28), fa.getWeightFor(t(3, "g", ACC)))
    assertEquals(WPDSPostStarTests.w(35), fa.getWeightFor(t(4, "h", ACC)))
  }

  @Test def oneCall(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPostStarTests.w(1)))
    pds.addRule(wpush(1, "b", 2, "call", "d", WPDSPostStarTests.w(2)))
    pds.addRule(wnormal(2, "call", 2, "e", WPDSPostStarTests.w(3)))
    pds.addRule(wpop(2, "e", 3, WPDSPostStarTests.w(4)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(WPDSPostStarTests.w(10), fa.getWeightFor(t(3, "d", ACC)))
  }

  @Test def twoCallOnlyReturnWeight(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPostStarTests.w(0)))
    pds.addRule(wpush(1, "b", 2, "call", "d", WPDSPostStarTests.w(0)))
    pds.addRule(wnormal(2, "call", 2, "e", WPDSPostStarTests.w(0)))
    pds.addRule(wpop(2, "e", 3, WPDSPostStarTests.w(4)))
    pds.addRule(wnormal(3, "d", 3, "f", WPDSPostStarTests.w(0)))
    pds.addRule(wpush(3, "f", 2, "call", "g", WPDSPostStarTests.w(0)))
    pds.addRule(wnormal(3, "g", 4, "h", WPDSPostStarTests.w(0)))
    val fa = waccepts(1, "a", WPDSPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(WPDSPostStarTests.w(4), fa.getWeightFor(t(3, "f", ACC)))
    assertEquals(WPDSPostStarTests.w(8), fa.getWeightFor(t(3, "g", ACC)))
    assertEquals(WPDSPostStarTests.w(8), fa.getWeightFor(t(4, "h", ACC)))
  }
}