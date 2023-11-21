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

object CastAndSetTest {
  private class Container {
    private[fields] var o: Nothing = null

    def set(o1: Nothing): Unit = {
      val `var` = o1.asInstanceOf[Nothing]
      this.o = `var`
    }

    def get: Nothing = o
  }
}

class CastAndSetTest extends Nothing {
  @Test def setAndGet(): Unit = {
    val container = new CastAndSetTest.Container
    val o1 = new Nothing
    container.set(o1)
    val o2 = new Nothing
    container.set(o2)
    val alias = container.get
    queryFor(alias)
  }
}