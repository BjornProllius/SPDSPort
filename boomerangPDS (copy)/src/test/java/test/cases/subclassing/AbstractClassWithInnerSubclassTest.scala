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
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object AbstractClassWithInnerSubclassTest {
  private class Superclass {
    private[subclassing] val e: AbstractClassWithInnerSubclassTest.Element = null
  }

  private class Subclass private[subclassing] extends AbstractClassWithInnerSubclassTest.Superclass {
    e = new AbstractClassWithInnerSubclassTest.Subclass#InnerClass

    private class InnerClass extends AbstractClassWithInnerSubclassTest.Element {
      private[subclassing] val c = new AbstractClassWithInnerSubclassTest.AnotherClass

      @Override override def get: AbstractClassWithInnerSubclassTest.AnotherClass = c
    }
  }

  private class AnotherClass {
    private[subclassing] val o = new Nothing() {}
  }

  private trait Element {
    def get: AbstractClassWithInnerSubclassTest.AnotherClass
  }
}

class AbstractClassWithInnerSubclassTest extends Nothing {
  @Test def typingIssue(): Unit = {
    val subclass2 = new AbstractClassWithInnerSubclassTest.Subclass
    val query = subclass2.e.get.o
    queryFor(query)
  }
}