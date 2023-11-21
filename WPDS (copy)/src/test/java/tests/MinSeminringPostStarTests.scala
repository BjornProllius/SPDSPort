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
import tests.TestHelper.s
import tests.TestHelper.t
import org.junit.Before
import org.junit.Test
import tests.TestHelper.Abstraction
import tests.TestHelper.StackSymbol
import wpds.impl.NormalRule
import wpds.impl.PopRule
import wpds.impl.PushRule
import wpds.impl.WeightedPAutomaton
import wpds.impl.WeightedPushdownSystem

object MinSeminringPostStarTests {
  private def w(i: Int) = new Nothing(i)

  private[tests] def waccepts(a: Int, c: Nothing, weight: Nothing) = {
    val aut = new Nothing() {
      @Override def createState(d: Nothing, loc: Nothing) = new Nothing(d, loc)

      @Override def epsilon: Nothing = s("EPS")

      @Override def getOne: Nothing = MinSemiring.one

      @Override def isGeneratedState(d: Nothing): Boolean = d.s != null
    }
    aut.addFinalState(ACC)
    aut.addTransition(t(a, c, ACC))
    aut.addWeightForTransition(t(a, c, ACC), weight)
    aut
  }

  private[tests] def wnormal(a: Int, n: Nothing, b: Int, m: Nothing, w: Nothing) = new Nothing(a(a), s(n), a(b), s(m), w)

  private[tests] def wpush(a: Int, n: Nothing, b: Int, m: Nothing, l: Nothing, w: Nothing) = new Nothing(a(a), s(n), a(b), s(m), s(l), w)

  private[tests] def wpop(a: Int, n: Nothing, b: Int, w: Nothing) = new Nothing(a(a), s(n), a(b), w)
}

class MinSeminringPostStarTests {
  private var pds: Nothing = null

  @Before def init(): Unit = {
    pds = new Nothing
  }

  @Test def simple(): Unit = {
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "a", 2, "b", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(2, "b", 3, "c", MinSeminringPostStarTests.w(1)))
    val fa = MinSeminringPostStarTests.waccepts(1, "a", MinSeminringPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(fa.getTransitions.size, 3)
    assertEquals(fa.getStates.size, 4)
    assertEquals(MinSeminringPostStarTests.w(2), fa.getWeightFor(t(3, "c", ACC)))
  }

  @Test def branch(): Unit = {
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "a", 1, "b", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "b", 1, "c", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "a", 1, "d", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "d", 1, "c", MinSeminringPostStarTests.w(1)))
    val fa = MinSeminringPostStarTests.waccepts(1, "a", MinSeminringPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(MinSeminringPostStarTests.w(2), fa.getWeightFor(t(1, "c", ACC)))
    assertEquals(MinSeminringPostStarTests.w(1), fa.getWeightFor(t(1, "b", ACC)))
    assertEquals(MinSeminringPostStarTests.w(1), fa.getWeightFor(t(1, "d", ACC)))
  }

  @Test def push1(): Unit = {
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "a", 1, "b", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpush(1, "b", 1, "c", "d", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "c", 1, "e", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpop(1, "e", 1, MinSeminringPostStarTests.w(1)))
    val fa = MinSeminringPostStarTests.waccepts(1, "a", MinSeminringPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(MinSeminringPostStarTests.w(1), fa.getWeightFor(t(1, "b", ACC)))
    assertEquals(MinSeminringPostStarTests.w(4), fa.getWeightFor(t(1, "d", ACC)))
    // assertEquals(w(2), fa.getWeightFor(t(1, "e", a(1, "c"))));
  }

  @Test def push2(): Unit = {
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "a", 2, "b", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpush(2, "b", 3, "c", "d", MinSeminringPostStarTests.w(2)))
    pds.addRule(MinSeminringPostStarTests.wnormal(3, "c", 4, "e", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpop(4, "e", 5, MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(5, "d", 2, "f", MinSeminringPostStarTests.w(10)))
    val fa = MinSeminringPostStarTests.waccepts(1, "a", MinSeminringPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(MinSeminringPostStarTests.w(5), fa.getWeightFor(t(5, "d", ACC)))
    assertEquals(MinSeminringPostStarTests.w(15), fa.getWeightFor(t(2, "f", ACC)))
  }

  @Test def twoCall(): Unit = {
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "a", 1, "b", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpush(1, "b", 1, "call", "d", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "call", 1, "e", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpop(1, "e", 1, MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "d", 1, "f", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wpush(1, "f", 1, "call", "g", MinSeminringPostStarTests.w(1)))
    pds.addRule(MinSeminringPostStarTests.wnormal(1, "g", 1, "h", MinSeminringPostStarTests.w(1)))
    val fa = MinSeminringPostStarTests.waccepts(1, "a", MinSeminringPostStarTests.w(0))
    pds.poststar(fa)
    assertEquals(MinSeminringPostStarTests.w(9), fa.getWeightFor(t(1, "h", ACC)))
  }
}