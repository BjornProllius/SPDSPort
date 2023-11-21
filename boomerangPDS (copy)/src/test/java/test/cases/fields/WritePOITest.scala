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

object WritePOITest {
  class A {
    private[fields] val b: Nothing = null
    // Alloc c = null;
  }

  class I {
    private[fields] val b: Nothing = null // = new Object();
  }

  class Level1 {
    private[fields] val l2 = new WritePOITest.Level2
  }

  class Level2 {
    private[fields] val a: WritePOITest.Alloc = null
  }

  def flowsToField(parameterContainer: WritePOITest.ObjectWithField): Unit = {
    val field = parameterContainer.field
    val aliasedVar = parameterContainer.field
    //        System.out.println(field);
    //        System.out.println(aliasedVar);
  }

  class ObjectWithField {
    private[fields] val field: Nothing = null
  }

  class Alloc extends Nothing {}

  private class A1 {
    private[fields] val b = new WritePOITest.B1
  }

  private class B1 {
    private[fields] val c: Nothing = null
  }
}

class WritePOITest extends Nothing {
  @Test def overwrite(): Unit = {
    val a = new WritePOITest.I
    a.b = new Nothing
    a.b = new WritePOITest.Alloc
    val alias = a.b
    queryFor(alias)
  }

  @Test def indirectAllocationSite12(): Unit = {
    val a = new WritePOITest.A
    setField(a)
    val alias = a.b
    queryFor(alias)
  }

  private def setField(a: WritePOITest.A): Unit = {
    val a1 = a
    val alloc = new WritePOITest.Alloc
    a1.b = alloc
  }

  @Test def indirectAllocationSite(): Unit = {
    val query = new WritePOITest.Alloc
    val a = new WritePOITest.A
    val e = a
    e.b = new Nothing
    a.b = query
    val alias = e.b
    queryFor(alias)
  }

  @Test def indirectAllocationSite1(): Unit = {
    val query = new WritePOITest.Alloc
    val a = new WritePOITest.A
    val e = a
    a.b = query
    val alias = e.b
    queryFor(alias)
  }

  @Test def indirectAllocationSite144(): Unit = {
    val q = new WritePOITest.Alloc
    val a = new WritePOITest.A
    val e = a
    a.b = q
    // Object alias = e.b;
    queryFor(q)
  }

  @Test def indirectAllocationSite2(): Unit = {
    val query = new WritePOITest.Alloc
    val a = new WritePOITest.A
    val e = a
    a.b = query
    val x = 1
    val alias = e.b
    queryFor(alias)
  }

  @Test def indirectAllocationSite3(): Unit = {
    val query = new WritePOITest.Alloc
    val a = new WritePOITest.A
    val e = a
    a.b = query
    queryFor(query)
  }

  @Test def doubleIndirectAllocationSite(): Unit = {
    val base = new WritePOITest.Level1
    val query = new WritePOITest.Alloc
    val level2 = new WritePOITest.Level2
    base.l2 = level2
    level2.a = query
    val intermediat = base.l2
    val samesame = intermediat.a
    queryFor(samesame)
  }

  @Test def doubleIndirectAllocationSiteSIMPLE(): Unit = {
    val base = new WritePOITest.Level1
    val query = new WritePOITest.Alloc
    val level2 = new WritePOITest.Level2
    base.l2 = level2
    level2.a = query
    queryFor(query)
  }

  @Test def simpleIndirectAllocationSite(): Unit = {
    val base = new WritePOITest.Level1
    val query = new WritePOITest.Alloc
    val level2 = new WritePOITest.Level2
    base.l2 = level2
    level2.a = query
    queryFor(query)
  }

  @Test def doubleIndirectAllocationSiteMoreComplex(): Unit = {
    val base = new WritePOITest.Level1
    val baseAlias = base
    val query = new WritePOITest.Alloc
    val level2 = new WritePOITest.Level2
    base.l2 = level2
    level2.a = query
    val alias = baseAlias.l2
    val samesame = alias.a
    queryFor(samesame)
  }

  @Test def directAllocationSite(): Unit = {
    val query = new WritePOITest.Alloc
    val a = new WritePOITest.A
    a.b = query
    val alias = a.b
    queryFor(alias)
  }

  @Test def directAllocationSiteSimpler(): Unit = {
    val query = new WritePOITest.Alloc
    val a = new WritePOITest.A
    a.b = query
    queryFor(query)
  }

  @Test def loadTwice(): Unit = {
    val alloc = new WritePOITest.Alloc
    val a = new WritePOITest.A
    a.b = alloc
    val query1 = a.b
    val query2 = a.b
    queryFor(query2)
  }

  @Test def overwriteTwice(): Unit = {
    // TODO This test case should not be imprecise, but is imprecise. Jimple introduces additional
    // variable, why?.
    val alloc = new WritePOITest.Alloc
    val a = new WritePOITest.A
    a.b = new Nothing
    a.b = alloc
    val x = 1
    val query1 = a.b
    queryFor(query1)
  }

  @Test def overwriteWithinCall(): Unit = {
    val alloc = new WritePOITest.Alloc
    val a = new WritePOITest.A
    set(a)
    a.b = alloc
    val x = 1
    val query1 = a.b
    queryFor(query1)
  }

  private def set(a: WritePOITest.A): Unit = {
    a.b = new Nothing
  }

  @Test def overwriteTwiceStrongAliased(): Unit = {
    // This test case is expected to be imprecise.
    val alloc = new WritePOITest.Alloc
    val a = new WritePOITest.A
    val b = a
    b.b = new Nothing
    b.b = alloc
    val query1 = b.b
    queryFor(query1)
  }

  @Test def test(): Unit = {
    val a = new WritePOITest.ObjectWithField
    val b = a
    val file = new WritePOITest.Alloc
    bar(a, file)
    val z = b.field
    queryFor(z)
  }

  private def bar(a: WritePOITest.ObjectWithField, file: WritePOITest.Alloc): Unit = {
    a.field = file
  }

  @Test def test2(): Unit = {
    val a = new WritePOITest.ObjectWithField
    val b = a
    val file = new WritePOITest.Alloc
    bar(a, b, file)
    queryFor(b.field)
  }

  @Test def fieldStoreAndLoad2(): Unit = {
    val container = new WritePOITest.ObjectWithField
    container.field = new WritePOITest.Alloc
    val otherContainer = new WritePOITest.ObjectWithField
    val a = container.field
    otherContainer.field = a
    WritePOITest.flowsToField(container)
    // mustBeInErrorState( container.field);
    queryFor(a)
  }

  def bar(a: WritePOITest.ObjectWithField, b: WritePOITest.ObjectWithField, file: WritePOITest.Alloc): Unit = {
    a.field = file
  }

  @Test def doubleNested(): Unit = {
    val a = new WritePOITest.A1
    val x = a.b
    x.c = new Nothing
    val y = a.b
    y.c = null
    val query = a.b.c
    queryFor(query)
  }

  @Test def doubleNestedBranched(): Unit = {
    val a = new WritePOITest.A1
    if (staticallyUnknown) a.b = new WritePOITest.B1
    else a.b = new WritePOITest.B1
    a.b.c = new Nothing
    val y = a.b
    y.c = null
    val query = a.b.c
    queryFor(query)
  }
}