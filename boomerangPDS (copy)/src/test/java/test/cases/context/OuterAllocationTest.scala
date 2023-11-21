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

object OuterAllocationTest {
  private class File extends Nothing {
    def open(): Unit = {
    }
  }

  private class ObjectWithField {
    private[context] val field: OuterAllocationTest.File = null
  }
}

class OuterAllocationTest extends Nothing {
  @Test def main(): Unit = {
    val container = new OuterAllocationTest.ObjectWithField
    container.field = new OuterAllocationTest.File
    val otherContainer = new OuterAllocationTest.ObjectWithField
    val a = container.field
    otherContainer.field = a
    flows(container)
  }

  private def flows(container: OuterAllocationTest.ObjectWithField): Unit = {
    val field = container.field
    field.open()
    queryFor(field)
  }
}