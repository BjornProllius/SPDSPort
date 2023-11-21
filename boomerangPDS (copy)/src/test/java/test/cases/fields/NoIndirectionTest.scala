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

class NoIndirectionTest extends Nothing {
  @Test def doubleWriteAndReadFieldPositive(): Unit = {
    val query = new Nothing
    val a = new Nothing
    val b = new Nothing
    a.b = query
    b.a = a
    val c = b.a
    val alias = c.b
    queryFor(alias)
  }

  @Test def doubleWriteAndReadFieldNegative(): Unit = {
    val query = new Nothing
    val a = new Nothing
    val b = new Nothing
    a.b = query
    b.a = a
    val c = b.a
    val alias = c.c
    unreachable(alias)
  }

  @Test def writeWithinCallPositive(): Unit = {
    val query = new Nothing
    val a = new Nothing
    call(a, query)
    val alias = a.b
    queryFor(alias)
  }

  @Test def writeWithinCallNegative(): Unit = {
    val query = new Nothing
    val a = new Nothing
    call(a, query)
    val alias = a.c
    unreachable(alias)
  }

  @Test def writeWithinCallSummarizedPositive(): Unit = {
    val query = new Nothing
    val a = new Nothing
    call(a, query)
    val alias = a.b
    val b = new Nothing
    call(b, alias)
    val summarizedAlias = b.b
    queryFor(summarizedAlias)
  }

  private def call(a: Nothing, query: Nothing): Unit = {
    a.b = query
  }

  @Test def doubleWriteWithinCallPositive(): Unit = {
    val query = new Nothing
    val a = new Nothing
    val b = callAndReturn(a, query)
    val first = b.a
    val alias = first.b
    queryFor(alias)
  }

  private def callAndReturn(a: Nothing, query: Nothing) = {
    a.b = query
    val b = new Nothing
    b.a = a
    b
  }

  @Test def overwriteFieldTest(): Unit = {
    val query = new Nothing
    val a = new Nothing
    a.b = query
    a.b = null
    val alias = a.b
    unreachable(alias)
  }

  @Test def overwriteButPositiveFieldTest(): Unit = {
    val query = new Nothing
    val a = new Nothing
    a.b = query
    // a.c = null;
    val alias = a.b
    queryFor(alias)
  }

  @Test def overwriteButPositiveFieldTest2(): Unit = {
    val query = new Nothing
    val x = 0
    val a = new Nothing
    a.b = query
    a.b = null
    val y = x
    val alias = a.b
    queryFor(alias)
  }
}