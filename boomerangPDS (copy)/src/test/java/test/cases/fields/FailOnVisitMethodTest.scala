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

import java.util
import java.util.Collections
import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject
import test.core.selfrunning.NullableField

object FailOnVisitMethodTest {
  private class E {
    def bar(): Unit = {
    }
  }

  private class O private {
    e = new FailOnVisitMethodTest.E
    e.bar()
    private[fields] val nullableField: FailOnVisitMethodTest.N = null
    private[fields] var e: FailOnVisitMethodTest.E = null
  }

  private class N extends Nothing {}
}

class FailOnVisitMethodTest extends Nothing {
  private class A {
    private[fields] val b = new FailOnVisitMethodTest#B
    private[fields] val e = new FailOnVisitMethodTest.E
  }

  private class B extends Nothing {}

  @Test def failOnVisitBar(): Unit = {
    val a = new FailOnVisitMethodTest#A
    val alias = a.b
    val e = a.e
    e.bar()
    queryFor(alias)
  }

  private class C {
    private[fields] val b: FailOnVisitMethodTest#B = null
    private[fields] val e: FailOnVisitMethodTest.E = null
  }

  @Test def failOnVisitBarSameMethod(): Unit = {
    val a = new FailOnVisitMethodTest#C
    a.b = new FailOnVisitMethodTest#B
    val alias = a.b
    val e = a.e
    e.bar()
    queryFor(alias)
  }

  @Test def failOnVisitBarSameMethodAlloc(): Unit = {
    val a = new FailOnVisitMethodTest#C
    a.b = new FailOnVisitMethodTest#B
    a.e = new FailOnVisitMethodTest.E
    val alias = a.b
    val e = a.e
    e.bar()
    queryFor(alias)
  }

  @Test def failOnVisitBarSameMethodSimpleAlloc(): Unit = {
    val a = new FailOnVisitMethodTest#Simplified
    a.e = new FailOnVisitMethodTest.E
    val alias = a.b
    val e = a.e
    e.bar()
    queryFor(alias)
  }

  private class Simplified {
    private[fields] val e: FailOnVisitMethodTest.E = null
    private[fields] val b: FailOnVisitMethodTest.N = null
  }

  @Test def doNotVisitBar(): Unit = {
    val a = new FailOnVisitMethodTest.O
    val alias = a.nullableField
    queryFor(alias)
  }

  @Override protected def errorOnVisitMethod: Nothing = Collections.singleton("<test.cases.fields.FailOnVisitMethodTest$E: void bar()>")
}