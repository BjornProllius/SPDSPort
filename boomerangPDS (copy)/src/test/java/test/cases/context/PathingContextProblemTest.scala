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
package test.cases.context

import org.junit.Test
import test.cases.basic.Allocation
import test.core.AbstractBoomerangTest

object PathingContextProblemTest {
  class Inner {
    def callee(a: Nothing, b: Nothing): Unit = {
      queryFor(a)
    }

    def test1(): Unit = {
      val a1 = new Nothing
      val b1 = a1
      callee(a1, b1)
    }

    def test2(): Unit = {
      val a2 = new Nothing
      val b2 = new Nothing
      callee(a2, b2)
    }
  }

  class Inner2 {
    def callee(a: Nothing, b: Nothing): Unit = {
      queryFor(b)
    }

    def test1(): Unit = {
      val a1 = new Nothing
      val b1 = a1
      callee(a1, b1)
    }

    def test2(): Unit = {
      val a2 = new Nothing
      val b2 = new Nothing
      callee(a2, b2)
    }
  }
}

class PathingContextProblemTest extends Nothing {
  @Test def start(): Unit = {
    val i = new PathingContextProblemTest.Inner
    i.test1()
    i.test2()
  }

  @Test def start2(): Unit = {
    val i = new PathingContextProblemTest.Inner
    i.test1()
    i.test2()
  }
}