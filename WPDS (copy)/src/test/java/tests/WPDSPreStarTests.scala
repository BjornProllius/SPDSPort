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
import tests.TestHelper.t
import tests.TestHelper.waccepts
import tests.TestHelper.wnormal
import tests.TestHelper.wpop
import tests.TestHelper.wpush
import org.junit.Before
import org.junit.Ignore
import org.junit.Test
import tests.TestHelper.Abstraction
import tests.TestHelper.StackSymbol
import wpds.impl.WeightedPAutomaton
import wpds.impl.WeightedPushdownSystem

@Ignore object WPDSPreStarTests {
  private def w(i: Int) = new Nothing(i)
}

@Ignore class WPDSPreStarTests {
  private var pds: Nothing = null

  @Before def init(): Unit = {
    pds = new Nothing
  }

  @Test def simple(): Unit = {
    pds.addRule(wnormal(1, "a", 2, "b", WPDSPreStarTests.w(2)))
    pds.addRule(wnormal(2, "b", 3, "c", WPDSPreStarTests.w(3)))
    val fa = waccepts(3, "c", WPDSPreStarTests.w(0))
    pds.prestar(fa)
    assertEquals(fa.getTransitions.size, 3)
    assertEquals(fa.getStates.size, 4)
    assertEquals(fa.getWeightFor(t(1, "a", ACC)), WPDSPreStarTests.w(5))
  }

  @Test def branch(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPreStarTests.w(2)))
    pds.addRule(wnormal(1, "b", 1, "c", WPDSPreStarTests.w(4)))
    pds.addRule(wnormal(1, "a", 1, "d", WPDSPreStarTests.w(3)))
    pds.addRule(wnormal(1, "d", 1, "c", WPDSPreStarTests.w(3)))
    val fa = waccepts(1, "c", WPDSPreStarTests.w(0))
    pds.prestar(fa)
    assertEquals(fa.getWeightFor(t(1, "a", ACC)), WPDSPreStarTests.w(6))
    assertEquals(fa.getWeightFor(t(1, "b", ACC)), WPDSPreStarTests.w(4))
    assertEquals(fa.getWeightFor(t(1, "d", ACC)), WPDSPreStarTests.w(3))
  }

  @Test def push1(): Unit = {
    pds.addRule(wnormal(1, "a", 1, "b", WPDSPreStarTests.w(2)))
    pds.addRule(wpush(1, "b", 1, "c", "d", WPDSPreStarTests.w(3)))
    pds.addRule(wnormal(1, "c", 1, "e", WPDSPreStarTests.w(1)))
    pds.addRule(wpop(1, "e", 1, WPDSPreStarTests.w(5)))
    pds.addRule(wnormal(1, "d", 1, "f", WPDSPreStarTests.w(6)))
    val fa = waccepts(1, "f", WPDSPreStarTests.w(0))
    pds.prestar(fa)
    assertEquals(fa.getWeightFor(t(1, "a", ACC)), WPDSPreStarTests.w(17))
    assertEquals(fa.getWeightFor(t(1, "b", ACC)), WPDSPreStarTests.w(15))
    assertEquals(fa.getWeightFor(t(1, "c", 1)), WPDSPreStarTests.w(6))
  }
}