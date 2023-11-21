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
package test.cases.accesspath

import org.junit.Ignore
import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

@Ignore object AccessPathTest {
  private class A {
    private[accesspath] val b: AccessPathTest.B = null
    private[accesspath] val g: AccessPathTest.A = null
  }

  private class AllocA extends AccessPathTest.A with Nothing {}

  private class B extends Nothing {
    private[accesspath] val c: AccessPathTest.B = null
    private[accesspath] val d: AccessPathTest.B = null
    var b: AccessPathTest.B = null
  }

  private class C {
    private[accesspath] val b: AccessPathTest.B = null
    private[accesspath] val attr = new AccessPathTest.A
  }

  private[accesspath] def use(b: Nothing): Unit = {
  }

  private def staticCallOnFile(x: AccessPathTest.ClassWithField, n: AccessPathTest.NestedClassWithField): Unit = {
    val queryVariable = x.field
    // The analysis triggers a query for the following variable
    accessPathQueryFor(queryVariable, "x[field];n[nested,field]")
  }

  class ClassWithField {
    var field: AccessPathTest.ObjectOfInterest = null
  }

  class ObjectOfInterest extends Nothing {}

  class NestedClassWithField {
    var nested: AccessPathTest.ClassWithField = null
  }
}

@Ignore class AccessPathTest extends Nothing {
  @Test def getAllAliases(): Unit = {
    val a = new AccessPathTest.A
    val alloc = new AccessPathTest.B
    a.b = alloc
    accessPathQueryFor(alloc, "a[b]")
  }

  @Test def sameField(): Unit = {
    val alloc = new AccessPathTest.AllocA
    val b = new AccessPathTest.A
    val c = new AccessPathTest.A
    b.g = alloc
    c.g = b
    accessPathQueryFor(alloc, "b[g];c[g,g]")
  }

  @Test def getAllAliasesBranched(): Unit = {
    val a = new AccessPathTest.A
    val b = new AccessPathTest.A
    val alloc = new AccessPathTest.B
    if (staticallyUnknown) a.b = alloc
    else b.b = alloc
    accessPathQueryFor(alloc, "a[b];b[b]")
  }

  @Ignore
  @Test def getAllAliasesLooped(): Unit = {
    val a = new AccessPathTest.A
    val alloc = new AccessPathTest.B
    a.b = alloc
    for (i <- 0 until 10) {
      val d = alloc
      alloc.c = d
    }
    accessPathQueryFor(alloc, "a[b];alloc[c]*")
  }

  @Ignore
  @Test def getAllAliasesLoopedComplex(): Unit = {
    val a = new AccessPathTest.A
    val alloc = new AccessPathTest.B
    a.b = alloc
    for (i <- 0 until 10) {
      val d = alloc
      if (staticallyUnknown) alloc.c = d
      if (staticallyUnknown) alloc.d = d
    }
    accessPathQueryFor(alloc, "a[b];alloc[c]*;alloc[d]*;alloc[c,d];alloc[d,c]")
  }

  @Test def simpleIndirect(): Unit = {
    val a = new AccessPathTest.A
    val b = a
    val alloc = new AccessPathTest.B
    a.b = alloc
    accessPathQueryFor(alloc, "a[b];b[b]")
  }

  @Test def doubleIndirect(): Unit = {
    val b = new AccessPathTest.C
    val alloc = new AccessPathTest.B
    b.attr.b = alloc
    accessPathQueryFor(alloc, "b[attr,b]")
  }

  @Test def contextQuery(): Unit = {
    val a = new AccessPathTest.B
    val b = a
    context(a, b)
  }

  private def context(a: AccessPathTest.B, b: AccessPathTest.B): Unit = {
    accessPathQueryFor(a, "a;b")
  }

  @Test def doubeContextQuery(): Unit = {
    val a = new AccessPathTest.B
    val b = a
    context1(a, b)
  }

  private def context1(a: AccessPathTest.B, b: AccessPathTest.B): Unit = {
    context(a, b)
  }

  @Test def twoLevelTest(): Unit = {
    val b = new AccessPathTest.C
    taintMe(b)
  }

  @Test def threeLevelTest(): Unit = {
    val b = new AccessPathTest.C
    taintOnNextLevel(b)
  }

  private def taintMe(b: AccessPathTest.C): Unit = {
    val alloc = new AccessPathTest.B
    b.attr.b = alloc
    accessPathQueryFor(alloc, "alloc;b[attr,b]")
  }

  private def taintOnNextLevel(b: AccessPathTest.C): Unit = {
    taintMe(b)
  }

  @Test def hiddenFieldLoad(): Unit = {
    val a = new AccessPathTest.ClassWithField
    a.field = new AccessPathTest.ObjectOfInterest
    val b = a
    val n = new AccessPathTest.NestedClassWithField
    n.nested = b
    AccessPathTest.staticCallOnFile(a, n)
  }

  @Test def hiddenFieldLoad2(): Unit = {
    val alloc = new AccessPathTest.ObjectOfInterest
    val n = new AccessPathTest.NestedClassWithField
    store(n, alloc)
    accessPathQueryFor(alloc, "n[nested,field]")
  }

  private def store(o1: AccessPathTest.NestedClassWithField, oOfInterest: AccessPathTest.ObjectOfInterest): Unit = {
    val a = new AccessPathTest.ClassWithField
    a.field = oOfInterest
    val b = a
    o1.nested = b
  }

  @Test def hiddenFieldLoad3(): Unit = {
    val alloc = new AccessPathTest.ObjectOfInterest
    val n = new AccessPathTest.NestedClassWithField
    val t = n
    store(n, alloc)
    accessPathQueryFor(alloc, "n[nested,field];t[nested,field]")
    AccessPathTest.use(t)
  }

  @Test def hiddenFieldLoad4(): Unit = {
    val alloc = new AccessPathTest.ObjectOfInterest
    val n = new AccessPathTest.NestedClassWithField
    val t = n
    store(n, alloc)
    load(t)
  }

  private def load(t: AccessPathTest.NestedClassWithField): Unit = {
    queryFor(t.nested.field)
  }
}