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

class ReturnPOITest extends Nothing {
  class A {
    private[fields] val b: ReturnPOITest#B = null
  }

  class B {
    private[fields] val c: ReturnPOITest#C = null
  }

  class C extends Nothing {}

  @Test def indirectAllocationSite(): Unit = {
    val a = new ReturnPOITest#B
    val e = a
    allocation(a)
    val alias = e.c
    val query = a.c
    queryFor(query)
  }

  private def allocation(a: ReturnPOITest#B): Unit = {
    val d = new ReturnPOITest#C
    a.c = d
  }

  @Test def unbalancedReturnPOI1(): Unit = {
    val a = new ReturnPOITest#C
    val b = new ReturnPOITest#B
    val c = b
    setField(b, a)
    val alias = c.c
    queryFor(a)
  }

  private def setField(a2: ReturnPOITest#B, a: ReturnPOITest#C): Unit = {
    a2.c = a
  }

  @Test def unbalancedReturnPOI3(): Unit = {
    val b = new ReturnPOITest#B
    val c = b
    setField(c)
    val query = c.c
    queryFor(query)
  }

  private def setField(c: ReturnPOITest#B): Unit = {
    c.c = new ReturnPOITest#C
  }

  @Test def whyRecursiveReturnPOIIsNecessary(): Unit = {
    val c = new ReturnPOITest#C
    val b = new ReturnPOITest#B
    val a = new ReturnPOITest#A
    val a2 = a
    a2.b = b
    val b2 = b
    setFieldTwo(a, c)
    val alias = a2.b.c
    queryFor(c)
  }

  @Test def whysRecursiveReturnPOIIsNecessary(): Unit = {
    val c = new ReturnPOITest#C
    val b = new ReturnPOITest#B
    val a = new ReturnPOITest#A
    val a2 = a
    a2.b = b
    val b2 = b
    setFieldTwo(a, c)
    val alias = a2.b.c
    queryFor(alias)
  }

  private def setFieldTwo(b: ReturnPOITest#A, a: ReturnPOITest#C): Unit = {
    b.b.c = a
  }

  @Test def whysRecursiveReturnPOIIsNecessary3Addressed(): Unit = {
    val x = new ReturnPOITest#C
    val y = new ReturnPOITest#B
    val z = new ReturnPOITest#A
    val aliasOuter = z
    aliasOuter.b = y
    setFieldTwo3Addresses(z, x)
    val l1 = aliasOuter.b
    val alias = l1.c
    queryFor(alias)
  }

  private def setFieldTwo3Addresses(base: ReturnPOITest#A, overwrite: ReturnPOITest#C): Unit = {
    val loaded = base.b
    loaded.c = overwrite
  }
}