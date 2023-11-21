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
package test.cases.reflection

import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object ReflectionTest {
  private class A {
    private[reflection] val field = new Nothing
  }
}

class ReflectionTest extends Nothing {
  @Test
  @throws[ClassNotFoundException]
  @throws[InstantiationException]
  @throws[IllegalAccessException]
  def bypassClassForName(): Unit = {
    val query = new Nothing
    val cls = Class.forName(classOf[ReflectionTest.A].getName)
    queryFor(query)
  }

  @Ignore
  @Test
  @throws[ClassNotFoundException]
  @throws[InstantiationException]
  @throws[IllegalAccessException]
  def loadObject(): Unit = {
    val cls = Class.forName(classOf[ReflectionTest.A].getName)
    val newInstance = cls.newInstance
    val a = newInstance.asInstanceOf[ReflectionTest.A]
    val query = a.field
    queryFor(query)
  }
}