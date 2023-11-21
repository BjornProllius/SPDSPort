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
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject
import test.core.selfrunning.NoAllocatedObject

object ArrayTest {
  class A extends Nothing {}

  class NoAllocation extends Nothing {}

  private class B {
    private[array] val f: Nothing = null
  }

  private class C {
    private[array] val f: Nothing = null
  }
}

class ArrayTest extends Nothing {
  @Test def simpleAssignment(): Unit = {
    val array = new Array[Nothing](3)
    val alias = new ArrayTest.A
    array(2) = alias
    val query = array(2)
    queryFor(query)
  }

  @Test def indexInsensitive(): Unit = {
    val array = new Array[Nothing](3)
    val alias1 = new ArrayTest.A
    val alias2 = new ArrayTest.NoAllocation
    array(1) = alias1
    array(2) = alias2
    val query = array(1)
    queryFor(query)
  }

  @Test def doubleArray(): Unit = {
    val array = new Array[Array[Nothing]](3, 3)
    array(1)(2) = new ArrayTest.A
    array(2)(3) = new Nothing
    val query = array(1)(2)
    queryFor(query)
  }

  @Test def doubleArray3Address(): Unit = {
    val array = new Array[Array[Nothing]](3, 3)
    val load = array(1)
    val alloc = new ArrayTest.A
    load(2) = alloc
    val unrelatedLoad = array(2)
    val unrelatedAlloc = new ArrayTest.NoAllocation
    unrelatedLoad(3) = unrelatedAlloc
    val els = array(1)
    val query = els(2)
    queryFor(query)
  }

  @Ignore
  @Test def threeDimensionalArray(): Unit = {
    val array = new Array[Array[Array[Nothing]]](3, 3, 1)
    array(1)(2)(1) = new ArrayTest.A
    array(2)(3)(0) = new Nothing
    val query = array(3)(3)(2)
    queryFor(query)
  }

  @Test def arrayCopyTest(): Unit = {
    val copiedArray = new Array[Nothing](3)
    val originalArray = new Array[Nothing](3)
    val alias = new ArrayTest.A
    originalArray(1) = alias
    System.arraycopy(originalArray, 0, copiedArray, 0, 1)
    val query = copiedArray(1)
    queryFor(query)
  }

  @Test def arrayWithTwoObjectAndFieldTest(): Unit = {
    val b = new ArrayTest.B
    b.f = new ArrayTest.A
    val a = new ArrayTest.C
    a.f = new Nothing
    val container = new Array[Nothing](2)
    container(0) = b
    container(1) = a
    val bAlias = container(0)
    val casted = bAlias.asInstanceOf[ArrayTest.B]
    val query = casted.f
    queryFor(query)
  }

  @Test def toCharArrayTest(): Unit = {
    val s = "password"
    val query = s.toCharArray
    val t = query
    queryFor(t)
  }
}