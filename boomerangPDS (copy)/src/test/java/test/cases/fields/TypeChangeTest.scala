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

object TypeChangeTest {
  class D {
    private[fields] val f = new Nothing
    private[fields] val d = new TypeChangeTest.D

    def getField: Nothing = {
      val varShouldBeThere = this.f
      varShouldBeThere
    }

    def getDoubleField: Nothing = d.getField
  }
}

class TypeChangeTest extends Nothing {
  @Test def returnValue(): Unit = {
    val f = new TypeChangeTest.D
    val amIThere = f.getField
    queryFor(amIThere)
  }

  @Test def doubleReturnValue(): Unit = {
    val f = new TypeChangeTest.D
    val t = f.getDoubleField
    queryFor(t)
  }

  @Test def returnValueAndBackCast(): Unit = {
    val f = new TypeChangeTest.D
    val t = f.getField
    val u = t.asInstanceOf[Nothing]
    queryFor(u)
  }
}