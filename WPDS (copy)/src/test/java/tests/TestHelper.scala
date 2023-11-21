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

import wpds.impl.NormalRule
import wpds.impl.PAutomaton
import wpds.impl.PopRule
import wpds.impl.PushRule
import wpds.impl.Transition
import wpds.impl.UNormalRule
import wpds.impl.UPopRule
import wpds.impl.UPushRule
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.Location
import wpds.interfaces.State

object TestHelper {
  private[tests] val ACC = a(999)

  private[tests] def accepts(a: Int, c: Nothing) = {
    val aut = new Nothing() {
      @Override def createState(d: TestHelper.Abstraction, loc: TestHelper.StackSymbol) = new TestHelper.Abstraction(d, loc)

      @Override def epsilon: TestHelper.StackSymbol = s("EPS")

      @Override def isGeneratedState(d: TestHelper.Abstraction): Boolean = d.s != null
    }
    aut.addFinalState(ACC)
    aut.addTransition(t(a, c, ACC))
    aut
  }

  private[tests] def waccepts(a: Int, c: Nothing, weight: Nothing) = {
    val aut = new Nothing() {
      @Override def createState(d: TestHelper.Abstraction, loc: TestHelper.StackSymbol) = new TestHelper.Abstraction(d, loc)

      @Override def epsilon: TestHelper.StackSymbol = s("EPS")

      @Override def getOne: Nothing = NumWeight.one

      @Override def isGeneratedState(d: TestHelper.Abstraction): Boolean = d.s != null
    }
    aut.addFinalState(ACC)
    aut.addTransition(t(a, c, ACC))
    aut.addWeightForTransition(t(a, c, ACC), weight)
    aut
  }

  private[tests] def a(a: Int) = new TestHelper.Abstraction(a)

  private[tests] def a(a: Int, b: Nothing) = new TestHelper.Abstraction(a(a), s(b))

  private[tests] def s(a: Nothing) = new TestHelper.StackSymbol(a)

  private[tests] def t(a: TestHelper.Abstraction, c: TestHelper.StackSymbol, b: TestHelper.Abstraction) = new Nothing(a, c, b)

  private[tests] def t(a: TestHelper.Abstraction, c: Nothing, b: TestHelper.Abstraction) = new Nothing(a, s(c), b)

  private[tests] def t(a: Int, c: TestHelper.StackSymbol, b: TestHelper.Abstraction) = t(a(a), c, b)

  private[tests] def t(a: Int, c: Nothing, b: TestHelper.Abstraction) = t(a, s(c), b)

  private[tests] def t(a: Int, c: Nothing, b: Int) = t(a, c, a(b))

  private[tests] def normal(a: Int, n: Nothing, b: Int, m: Nothing) = new Nothing(a(a), s(n), a(b), s(m))

  private[tests] def push(a: Int, n: Nothing, b: Int, m: Nothing, l: Nothing) = new Nothing(a(a), s(n), a(b), s(m), s(l))

  private[tests] def pop(a: Int, n: Nothing, b: Int) = new Nothing(a(a), s(n), a(b))

  private[tests] def wnormal(a: Int, n: Nothing, b: Int, m: Nothing, w: Nothing) = new Nothing(a(a), s(n), a(b), s(m), w)

  private[tests] def wpush(a: Int, n: Nothing, b: Int, m: Nothing, l: Nothing, w: Nothing) = new Nothing(a(a), s(n), a(b), s(m), s(l), w)

  private[tests] def wpop(a: Int, n: Nothing, b: Int, w: Nothing) = new Nothing(a(a), s(n), a(b), w)

  private[tests] class Abstraction extends Nothing {
    final private[tests] val a = 0
    final private[tests] val s: TestHelper.StackSymbol = null

    def this(a: Int) {
      this()
      this.a = a
      this.s = null
    }

    def this(a: TestHelper.Abstraction, s: TestHelper.StackSymbol) {
      this()
      this.s = s
      this.a = a.a
    }

    @Override def toString: Nothing = if (s == null) Integer.toString(a)
    else "<" + a + "," + s + ">"

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + a
      result = prime * result + (if (s == null) 0
      else s.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[TestHelper.Abstraction]
      if (a != other.a) return false
      if (s == null) if (other.s != null) return false
      else if (!(s == other.s)) return false
      true
    }
  }

  private[tests] class StackSymbol private[tests](private[tests] val s: Nothing) extends Nothing {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (s == null) 0
      else s.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[TestHelper.StackSymbol]
      if (s == null) if (other.s != null) return false
      else if (!s.equals(other.s)) return false
      true
    }

    @Override def toString: Nothing = s

    @Override def accepts(other: Nothing): Boolean = this == other
  }
}