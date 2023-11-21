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

class InterproceduralTest extends Nothing {
  @Test def test3(): Unit = {
    val a = new InterproceduralTest#A
    val b = new InterproceduralTest#B
    b.c = new InterproceduralTest#C
    alias(a, b)
    val h = a.b
    val query = h.c
    queryFor(query)
  }

  private def alias(a: InterproceduralTest#A, b: InterproceduralTest#B): Unit = {
    a.b = b
  }

  class A {
    private[fields] val b: InterproceduralTest#B = null
  }

  class B {
    private[fields] val c: InterproceduralTest#C = null
  }

  class C extends Nothing {}
}