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
package test.cases.subclassing

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class InnerClass2Test extends Nothing {
  def doThings(name: Nothing): Unit = {
    class MyInner {
      def seeOuter(): Unit = {
        queryFor(name)
      }
    }
    val inner = new MyInner
    inner.seeOuter()
  }

  @Test def run(): Unit = {
    var alloc = new InnerClass2Test#Allocation
    val cmd = System.getProperty("")
    if (cmd != null) alloc = new InnerClass2Test#Allocation
    val outer = new InnerClass2Test
    outer.doThings(alloc)
  }

  private class Allocation extends Nothing {}
}