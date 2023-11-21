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
package test.cases.fields

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object CallPOITest {
  class A {
    private[fields] val b = new CallPOITest.B
  }

  class B {
    private[fields] val c: CallPOITest.AllocObj = null // = new AllocObj();
  }

  class AllocObj extends Nothing {}

  class T private[fields](private var value: Nothing) {
    def foo(s: CallPOITest.S): Unit = {
      val `val` = this.value
      s.set(`val`)
    }
  }

  class S {
    private var valueInS: Nothing = null

    def set(`val`: Nothing): Unit = {
      this.valueInS = `val`
    }

    def get: Nothing = {
      val alias = this.valueInS
      alias
    }
  }

  private def allocation(a: CallPOITest.A): Unit = {
    val intermediate = a.b
    val e = new CallPOITest.AllocObj
    val d = e
    intermediate.c = d
  }

  class Outer {
    private[fields] val field: Nothing = null
  }

  def fromIt(it: CallPOITest.SomeObj): Nothing = {
    val x = it.f
    x
  }

  def front(y: Nothing): Nothing = {
    val it = new CallPOITest.SomeObj
    val it2 = it
    it2.f = y
    val f = fromIt(it)
    f
  }

  class SomeObj {
    private[fields] val f: Nothing = null
  }

  class A1 {
    private[fields] val b: CallPOITest.B1 = null
  }

  class B1 {
    private[fields] val c: CallPOITest.AllocObj1 = null
  }

  class AllocObj1 extends Nothing {}
}

class CallPOITest extends Nothing {
  @Test def simpleButDiffer(): Unit = {
    val c = new Nothing
    val t = new CallPOITest.T(c)
    val s = new CallPOITest.S
    t.foo(s)
    val q = s.get
    queryFor(q)
  }

  @Test def indirectAllocationSite3Address(): Unit = {
    val a = new CallPOITest.A
    CallPOITest.allocation(a)
    val load = a.b
    val alias = load.c
    queryFor(alias)
  }

  @Test def indirectAllocationSiteViaParameter(): Unit = {
    val a = new CallPOITest.A
    val alloc = new CallPOITest.AllocObj
    allocation(a, alloc)
    val alias = a.b.c
    queryFor(alias)
  }

  @Test def indirectAllocationSiteViaParameterAliased(): Unit = {
    val a = new CallPOITest.A
    // a.b = new B();
    val alloc = new CallPOITest.AllocObj
    val b = a
    allocation(a, alloc)
    val loadedFromB = b.b
    val alias = loadedFromB.c
    queryFor(alias)
  }

  private def allocation(a: CallPOITest.A, d: CallPOITest.AllocObj): Unit = {
    val intermediate = a.b
    intermediate.c = d
  }

  @Test def whyRecursiveCallPOIIsNecessary(): Unit = {
    // TODO This test case seems to be non deterministic, why?
    val a = new CallPOITest.A
    val alloc = new CallPOITest.AllocObj
    val b = a
    allocationIndirect(a, alloc)
    val loadedFromB = b.b
    val alias = loadedFromB.c
    queryFor(alias)
  }

  @Test def whyRecursiveCallPOIIsNecessarySimpler(): Unit = {
    val a = new CallPOITest.A
    val alloc = new CallPOITest.AllocObj
    allocationIndirect(a, alloc)
    val alias = a.b.c
    queryFor(alias)
  }

  private def allocationIndirect(innerA: CallPOITest.A, d: CallPOITest.AllocObj): Unit = {
    val innerB = new CallPOITest.B
    val a2 = innerA
    a2.b = innerB
    val intermediate = a2.b
    intermediate.c = d
    val AndMe = innerA.b.c
  }

  @Test def whyRecursiveCallPOIIsNecessarySimpler2(): Unit = {
    val a = new CallPOITest.A
    val alloc = new CallPOITest.AllocObj
    allocationIndirect2(a, alloc)
  }

  private def allocationIndirect2(whereAmI: CallPOITest.A, d: CallPOITest.AllocObj): Unit = {
    val b = new CallPOITest.B
    val a2 = whereAmI
    a2.b = b
    val intermediate = a2.b
    intermediate.c = d
    val AndMe = whereAmI.b.c
    queryFor(AndMe)
  }

  @Test def innerSetFieldOnAlias(): Unit = {
    val o = new CallPOITest.Outer
    set(o)
    val alias = o.field
    queryFor(alias)
  }

  private def set(o: CallPOITest.Outer): Unit = {
    val alloc = new Nothing
    val alias = o
    alias.field = alloc
  }

  @Test def indirectAllocationSiteViaParameterAliasedNoPreAllocs(): Unit = {
    val a = new CallPOITest.A1
    a.b = new CallPOITest.B1
    val alloc = new CallPOITest.AllocObj1
    val b = a
    allocation(a, alloc)
    val loadedFromB = b.b
    val alias = loadedFromB.c
    queryFor(alias)
  }

  @Test def testForBackwardCallPOI(): Unit = {
    // Thanks to Martin Mory for contributing the test.
    val v = new Nothing
    val x = CallPOITest.front(v)
    queryFor(x)
  }

  private def allocation(a: CallPOITest.A1, d: CallPOITest.AllocObj1): Unit = {
    val intermediate = a.b
    intermediate.c = d
  }
}