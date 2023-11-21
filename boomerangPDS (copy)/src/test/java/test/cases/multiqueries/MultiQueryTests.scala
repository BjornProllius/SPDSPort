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
package test.cases.multiqueries

import org.junit.Test
import test.core.MultiQueryBoomerangTest
import test.core.selfrunning.AllocatedObject
import test.core.selfrunning.AllocatedObject2

object MultiQueryTests {
  private class Alloc1 extends Nothing {
    private[multiqueries] val field = new Nothing
  }

  private class Alloc2 extends Nothing {}
}

class MultiQueryTests extends Nothing {
  @Test def twoQueriesTest(): Unit = {
    val alloc1 = new MultiQueryTests.Alloc1
    val alias1 = new MultiQueryTests.Alloc2
    val query = alloc1
    queryFor1(query, classOf[Nothing])
    queryFor2(alias1, classOf[Nothing])
  }

  @Test def withFields(): Unit = {
    val alloc1 = new MultiQueryTests.Alloc1
    val alias1 = new MultiQueryTests.Alloc2
    val alias = alloc1
    alias.field = alias1
    val query = alloc1.field
    queryFor1(alias, classOf[Nothing])
    queryFor2(query, classOf[Nothing])
  }
}