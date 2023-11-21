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
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object AliasViaParameterTest {
  class A {
    private[context] val field: Nothing = null
  }
}

class AliasViaParameterTest extends Nothing {
  @Test def aliasViaParameter(): Unit = {
    val a = new AliasViaParameterTest.A
    val b = a
    setAndLoadFieldOnAlias(a, b)
    val query = a.field
    queryFor(query)
  }

  @Test def aliasViaParameterWrapped(): Unit = {
    val a = new AliasViaParameterTest.A
    val b = a
    passThrough(a, b)
    val query = a.field
    queryFor(query)
  }

  private def passThrough(a: AliasViaParameterTest.A, b: AliasViaParameterTest.A): Unit = {
    setAndLoadFieldOnAlias(a, b)
  }

  private def setAndLoadFieldOnAlias(a: AliasViaParameterTest.A, b: AliasViaParameterTest.A): Unit = {
    b.field = new Nothing() {}
  }
}