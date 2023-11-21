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

class MeetPOITest extends Nothing {
  @Test def wrappedAlloc(): Unit = {
    val e = new MeetPOITest#A
    val g = e
    wrapper(g)
    val h = e.b.c
    queryFor(h)
  }

  private def wrapper(g: MeetPOITest#A): Unit = {
    alloc(g)
  }

  private def alloc(g: MeetPOITest#A): Unit = {
    g.b.c = new MeetPOITest#C
  }

  class A {
    private[fields] val b = new MeetPOITest#B
  }

  class B {
    private[fields] val c: MeetPOITest#C = null
  }

  class C extends Nothing {
    private[fields] val g: Nothing = null
  }
}