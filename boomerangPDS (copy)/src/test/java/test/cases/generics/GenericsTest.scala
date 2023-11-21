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
package test.cases.generics

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object GenericsTest {
  class GenericClass[T] {
    private[generics] val field: T = null

    def setField(t: T): Unit = {
      field = t
    }

    def getField: T = field
  }

  class GenericType extends Nothing {}

  class WrappedGenericClass[T] {
    private[generics] val gen = new GenericsTest.GenericClass[T]

    def setField(t: T): Unit = {
      gen.setField(t)
    }

    def getField: T = gen.getField
  }
}

class GenericsTest extends Nothing {
  @Test def genericFieldAccess(): Unit = {
    val c = new GenericsTest.GenericClass[GenericsTest.GenericType]
    val genType = new GenericsTest.GenericType
    c.setField(genType)
    val query = c.getField
    queryFor(query)
  }

  @Test def genericFieldAccessWrapped(): Unit = {
    val c = new GenericsTest.WrappedGenericClass[GenericsTest.GenericType]
    val genType = new GenericsTest.GenericType
    c.setField(genType)
    val query = c.getField
    queryFor(query)
  }
}