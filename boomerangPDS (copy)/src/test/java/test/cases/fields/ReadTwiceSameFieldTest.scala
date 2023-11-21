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

class ReadTwiceSameFieldTest extends Nothing {
  @Test def recursiveTest(): Unit = {
    val a = new ReadTwiceSameFieldTest#Container
    val c = a.d
    val alias = c.d
    queryFor(alias)
  }

  @Test def readFieldTwice(): Unit = {
    val a = new ReadTwiceSameFieldTest#Container
    val c = a.d
    val alias = c.d
    queryFor(alias)
  }

  private class Container private[fields] {
    if (staticallyUnknown) d = new ReadTwiceSameFieldTest#Alloc
    else d = null
    private[fields] var d: ReadTwiceSameFieldTest#Container = null
  }

  private class DeterministicContainer private[fields] {
    d = new ReadTwiceSameFieldTest#DeterministicAlloc
    private[fields] var d: ReadTwiceSameFieldTest#DeterministicContainer = null
  }

  private class DeterministicAlloc extends ReadTwiceSameFieldTest#DeterministicContainer with Nothing {}

  private class Alloc extends ReadTwiceSameFieldTest#Container with Nothing {}
}