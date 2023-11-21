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

object HiddenFieldLoadTest {
  private class A {
    private[fields] var f: Nothing = null

    def setF(): Unit = {
      f = new Nothing
    }

    def setFBranched(): Unit = {
      if (staticallyUnknown) f = new Nothing
      else f = new Nothing
    }

    def staticallyUnknown = true

    def setF(alloc: Nothing): Unit = {
      f = alloc
    }

    def f: Nothing = f
  }
}

class HiddenFieldLoadTest extends Nothing {
  @Test def run(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    b.setF()
    val x = 1
    val alias = a.f
    queryFor(alias)
  }

  @Test def run1(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    b.setF()
    val x = 1
    val alias = a.f
    queryFor(alias)
  }

  @Test def run7(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    b.setFBranched()
    val x = 1
    val alias = a.f
    queryFor(alias)
  }

  @Test def run3(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    val alloc = new Nothing
    b.setF(alloc)
    // int x =1;
    val alias = a.f
    queryFor(alias)
  }

  @Test def run6(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    val allocInRun6 = new Nothing
    b.setF(allocInRun6)
    val x = 1
    val alias = a.f
    queryFor(alias)
  }

  @Test def run2(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    val c = new Nothing
    val y = 1
    b.f = c
    val x = 1
    val alias = a.f
    queryFor(alias)
  }

  @Test def run4(): Unit = {
    val b = new HiddenFieldLoadTest.A
    val a = b
    b.f = new Nothing
    val alias = a.f
    queryFor(alias)
  }
}