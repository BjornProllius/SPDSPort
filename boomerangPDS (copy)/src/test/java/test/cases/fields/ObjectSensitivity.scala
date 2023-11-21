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

object ObjectSensitivity {
  class A {
    var f: Nothing = null

    def this(o: Nothing) {
      this()
      this.f = o
    }

    def setF(b2: Nothing): Unit = {
      this.f = b2
    }

    def getF: Nothing = this.f
  }
}

class ObjectSensitivity extends Nothing {
  @Test def objectSensitivity0(): Unit = {
    val b1 = new Nothing
    val b2 = new Nothing
    val a1 = new ObjectSensitivity.A
    val a2 = new ObjectSensitivity.A
    a1.f = b1
    a2.f = b2
    val b3 = a1.getF
    val x = 1
    val b4 = a2.getF
    // flow(b4);
    queryFor(b4)
  }

  @Test def objectSensitivity1(): Unit = {
    val b1 = new Nothing
    val b2 = new Nothing
    val a1 = new ObjectSensitivity.A(b1)
    val a2 = new ObjectSensitivity.A(b2)
    val b3 = a1.getF
    val b4 = a2.getF
    // flow(b4);
    queryFor(b4)
  }

  private def flow(b3: Nothing): Unit = {

    // TODO Auto-generated method stub
  }

  @Test def objectSensitivity2(): Unit = {
    val b2 = new Nothing
    val a2 = new ObjectSensitivity.A(b2)
    otherScope()
    val b4 = a2.getF
    queryFor(b4)
  }

  private def otherScope(): Unit = {
    val b1 = new Nothing
    val a1 = new ObjectSensitivity.A(b1)
    val b3 = a1.getF
  }
}