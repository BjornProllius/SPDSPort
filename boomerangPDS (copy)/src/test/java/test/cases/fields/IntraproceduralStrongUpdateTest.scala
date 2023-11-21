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

object IntraproceduralStrongUpdateTest {
  private class B {
    private[fields] val field: Nothing = null
  }
}

class IntraproceduralStrongUpdateTest extends Nothing {
  @Test def strongUpdateWithField(): Unit = {
    val a = new IntraproceduralStrongUpdateTest#A
    a.field = new Nothing
    val b = a
    b.field = new Nothing() {}
    val alias = a.field
    queryFor(alias)
  }

  @Test def strongUpdateWithFieldSwapped(): Unit = {
    val a = new IntraproceduralStrongUpdateTest#A
    val b = a
    b.field = new Nothing
    a.field = new Nothing() {}
    val alias = a.field
    queryFor(alias)
  }

  private class A {
    private[fields] val field: Nothing = null
  }

  @Test def innerClass(): Unit = {
    val a = new IntraproceduralStrongUpdateTest#A
    val b = a
    b.field = new IntraproceduralStrongUpdateTest#I
    val alias = a.field
    queryFor(alias)
  }

  private class I extends Nothing {}

  @Test def anonymousClass(): Unit = {
    val a = new IntraproceduralStrongUpdateTest.B
    val b = a
    b.field = new Nothing() {}
    val alias = a.field
    queryFor(alias)
  }
}