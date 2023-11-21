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
package test.cases.array

import org.junit.Ignore
import org.junit.Test
import test.cases.array.ArrayIndexSensitiveTest.NoAllocation
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object ArrayContainerTest {
  private class ArrayContainer {
    private[array] val array = new Array[Nothing]

    private[array] def put(o: Nothing): Unit = {
      array(0) = o.asInstanceOf[Nothing]
    }

    private[array] def get = array(0)
  }

  class ArrayContainerWithPublicFields {
    var array = new Array[Nothing]
  }

  private class ArrayOfArrayOfContainers {
    private[array] val outerArray = Array[ArrayContainerTest.ArrayContainer](new ArrayContainerTest.ArrayContainer)

    private[array] def put(other: ArrayContainerTest.ArrayContainer): Unit = {
      outerArray(0) = other
    }

    private[array] def get = outerArray(0)
  }
}

class ArrayContainerTest extends Nothing {
  @Test def insertAndGet(): Unit = {
    val container = new ArrayContainerTest.ArrayContainer
    val o1 = new Nothing
    container.put(o1)
    val o2 = new Nothing
    container.put(o2)
    val alias = container.get
    queryFor(alias)
  }

  @Test def insertAndGetField(): Unit = {
    val container = new ArrayContainerTest.ArrayContainerWithPublicFields
    val o2 = new Nothing
    container.array(0) = o2
    val alias = container.array(0)
    queryFor(alias)
  }

  @Ignore
  @Test def insertAndGetDouble(): Unit = {
    val outerContainer = new ArrayContainerTest.ArrayOfArrayOfContainers
    val innerContainer1 = new ArrayContainerTest.ArrayContainer
    val o1 = new Nothing
    innerContainer1.put(o1)
    val o2 = new Nothing
    innerContainer1.put(o2)
    outerContainer.put(innerContainer1)
    val innerContainer2 = outerContainer.get
    val alias = innerContainer2.get
    queryFor(alias)
  }
}