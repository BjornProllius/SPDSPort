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

object SubclassingTest {
  private class Superclass {
    private[subclassing] val o = new Nothing() {}
  }

  private class Subclass extends SubclassingTest.Superclass {}

  private class ClassWithSubclassField(private[subclassing] var f: SubclassingTest.Subclass) {
  }
}

class SubclassingTest extends Nothing {
  @Test def typingIssue(): Unit = {
    val subclass = new SubclassingTest.Subclass
    val classWithSubclassField = new SubclassingTest.ClassWithSubclassField(subclass)
    val query = classWithSubclassField.f.o
    queryFor(query)
  }
}