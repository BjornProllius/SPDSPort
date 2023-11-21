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
package test.cases.integers

import java.math.BigInteger
import org.junit.Ignore
import org.junit.Test
import test.core.AbstractBoomerangTest

@Ignore class IntTest extends Nothing {
  @Test def simpleAssign(): Unit = {
    val allocation = 1
    intQueryFor(allocation, "1")
  }

  @Test def simpleAssignBranched(): Unit = {
    var allocation = 2
    if (staticallyUnknown) allocation = 1
    intQueryFor(allocation, "1,2")
  }

  @Test def simpleIntraAssign(): Unit = {
    val allocation = 1
    val y = allocation
    intQueryFor(y, "1")
  }

  @Test def simpleInterAssign(): Unit = {
    val allocation = 1
    val y = foo(allocation)
    intQueryFor(y, "1")
  }

  @Test def returnDirect(): Unit = {
    val allocation = getVal
    intQueryFor(allocation, "1")
  }

  @Test def returnInDirect(): Unit = {
    val x = getValIndirect
    intQueryFor(x, "1")
  }

  private def getValIndirect = {
    val allocation = 1
    allocation
  }

  private def getVal = 1

  private def foo(x: Int) = {
    val y = x
    y
  }

  @Ignore
  @Test def wrappedType(): Unit = {
    val integer = new Nothing(1)
    val allocation = integer
    intQueryFor(allocation, "1")
  }

  @Test def wrappedTypeBigInteger(): Unit = {
    val integer = BigInteger.valueOf(1)
    intQueryFor(integer, "1L")
  }
}