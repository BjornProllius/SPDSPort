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

object ReuseOfSummaryTest {
  private class A {
    private[fields] val f: Nothing = null
  }
}

class ReuseOfSummaryTest extends Nothing {
  @Test def summaryTest(): Unit = {
    val a = new ReuseOfSummaryTest.A
    val b = new ReuseOfSummaryTest.A
    val c = new Nothing // o1
    foo(a, b, c)
    foo(a, a, c)
    /**
     * the test case extracts all allocated object of type Alloc and assumes these objects to flow
     * as argument to queryFor(var). In this example var and a.f point to o1
     */
    val `var` = a.f
    queryFor(`var`)
  }

  private def foo(c: ReuseOfSummaryTest.A, d: ReuseOfSummaryTest.A, f: Nothing): Unit = {
    d.f = f
  }
}