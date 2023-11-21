/**
 * ***************************************************************************** Copyright (c) 2020
 * CodeShield GmbH, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package test.cases.array

import org.junit.Test
import test.cases.array.ArrayTest.NoAllocation
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject
import test.core.selfrunning.NoAllocatedObject

object ArrayIndexSensitiveTest {
  class Allocation extends Nothing {}

  class NoAllocation extends Nothing {}
}

class ArrayIndexSensitiveTest extends Nothing {
  @Test def simpleAssignment(): Unit = {
    val array = new Array[Nothing](3)
    val alloc = new ArrayIndexSensitiveTest.Allocation
    val alias = new ArrayIndexSensitiveTest.NoAllocation
    array(1) = alias
    array(2) = alloc
    val query = array(2)
    queryFor(query)
  }

  @Test def arrayIndexOverwrite(): Unit = {
    val array = new Array[Nothing](3)
    array(1) = new ArrayIndexSensitiveTest.NoAllocation
    val allocation = new ArrayIndexSensitiveTest.Allocation
    array(1) = allocation
    val query = array(1)
    queryFor(query)
  }

  @Test def arrayIndexNoOverwrite(): Unit = {
    val array = new Array[Nothing](3)
    array(1) = new ArrayIndexSensitiveTest.Allocation
    val noAlloc = new ArrayIndexSensitiveTest.NoAllocation
    array(2) = noAlloc
    val query = array(1)
    queryFor(query)
  }

  @Test def arrayLoadInLoop(): Unit = {
    val array = new Array[Nothing](3)
    array(0) = new ArrayIndexSensitiveTest.NoAllocation
    array(0) = new ArrayIndexSensitiveTest.Allocation
    array(1) = new ArrayIndexSensitiveTest.Allocation
    array(2) = new ArrayIndexSensitiveTest.Allocation // bw: pop 2, fw: push 2
    var q: Nothing = null
    for (i <- 0 until 3) {
      q = array(i) // bw: push ALL, fw: pop ALL
    }
    queryFor(q)
  }

  @Test def arrayStoreInLoop(): Unit = {
    val array = new Array[Nothing](3)
    val q = new ArrayIndexSensitiveTest.Allocation
    for (i <- 0 until 3) {
      array(i) = q // bw: pop ALL, fw: push ALL
    }
    val query = array(1) // bw: push 1, fw: pop 1
    queryFor(query)
  }

  @Test def copyArray(): Unit = {
    val array = new Array[Nothing](3)
    array(0) = new ArrayIndexSensitiveTest.NoAllocation
    array(0) = new ArrayIndexSensitiveTest.NoAllocation
    array(1) = new ArrayIndexSensitiveTest.Allocation
    array(2) = new ArrayIndexSensitiveTest.NoAllocation // bw: pop 2, fw: push 2
    val array2 = array
    val q = array2(1)
    queryFor(q)
  }
}