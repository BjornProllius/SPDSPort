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
package test.cases.synchronizd

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class BlockTest extends Nothing {
  private var field: Nothing = null

  @Test def block(): Unit = {
    field.synchronized {
      val o = new Nothing
      queryFor(o)
    }
  }

  @Test def block2(): Unit = {
    set()
    field.synchronized {
      val o = field
      queryFor(o)
    }
  }

  private def set(): Unit = {
    field.synchronized {
      field = new Nothing
    }
  }
}