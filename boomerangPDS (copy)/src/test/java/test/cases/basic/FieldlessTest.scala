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
package test.cases.basic

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class FieldlessTest extends Nothing {
  @Test def simpleAssignment1(): Unit = {
    val alloc1 = new Nothing
    val alias1 = alloc1
    val query = alias1
    queryFor(query)
  }

  @Test def simpleAssignment2(): Unit = {
    val alias1 = new Nothing() {}
    val b: Nothing = null
    var c: Nothing = null
    var alias2: Nothing = null
    var alias3: Nothing = null
    alias2 = alias1
    c = new Nothing
    alias3 = alias1
    queryFor(alias3)
  }

  @Test def branchWithOverwrite(): Unit = {
    var alias2 = new Nothing() {}
    if (staticallyUnknown) {
      val alias1 = alias2
      alias2 = new Nothing
    }
    queryFor(alias2)
  }

  @Test def branchWithOverwriteSwapped(): Unit = {
    var alias2 = new Nothing
    val alias1 = new Nothing
    if (staticallyUnknown) alias2 = alias1
    queryFor(alias2)
  }

  @Test def returnNullAllocation(): Unit = {
    val alias2 = returnNull
    queryFor(alias2)
  }

  private def returnNull: Nothing = {
    val x = new Nothing
    null
  }

  @Test def cast(): Unit = {
    val alias1 = new FieldlessTest#Subclass
    val alias2 = alias1.asInstanceOf[FieldlessTest#Subclass]
    queryFor(alias2)
  }

  // TODO when changed to private call graph removes edges.
  class Subclass extends Nothing {}

  def create: Nothing = {
    val alloc1 = new Nothing() {}
    alloc1
  }
}