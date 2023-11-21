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
package test.cases.subclassing

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object InnerClassTest {
  class Instance {
    var o = new Nothing

    class Inner {
      def getOuter: Nothing = thisInstance.o
    }
  }

  private class Instance2 {
    private var o: Nothing = null

    private class Inner {
      private def getOuter = thisInstance2.o

      private def setOuter(): Unit = {
        thisInstance2.o = new Nothing() {}
      }
    }
  }
}

class InnerClassTest extends Nothing {
  @Test def getFromInnerClass(): Unit = {
    val instance = new InnerClassTest.Instance
    val inner = new InnerClassTest.Instance#Inner
    val outer = inner.getOuter
    queryFor(outer)
  }

  @Test def getFromInnerClass2(): Unit = {
    val instance = new InnerClassTest.Instance2
    val inner = new InnerClassTest.Instance2#Inner
    inner.setOuter()
    val outer = inner.getOuter
    queryFor(outer)
  }
}