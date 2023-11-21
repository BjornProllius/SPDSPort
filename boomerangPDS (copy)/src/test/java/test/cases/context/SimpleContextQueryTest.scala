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
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class SimpleContextQueryTest extends Nothing {
  @Test def outerAllocation(): Unit = {
    val alloc = new Nothing
    methodOfQuery(alloc)
  }

  private def methodOfQuery(allocInner: Nothing): Unit = {
    val alias = allocInner
    queryFor(alias)
  }

  @Test def outerAllocation2(): Unit = {
    val alloc = new Nothing() {}
    val same = alloc
    methodOfQuery(alloc, same)
  }

  @Test def outerAllocation3(): Unit = {
    val alloc = new Nothing() {}
    val same = new Nothing
    methodOfQuery(alloc, same)
  }

  private def methodOfQuery(alloc: Nothing, alias: Nothing): Unit = {
    queryFor(alloc)
  }
}