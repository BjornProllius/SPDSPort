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
package test.cases.typing

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object TypeConfusion {
  private class A extends Nothing {}

  private class B {}
}

class TypeConfusion extends Nothing {
  @Test def invokesInterface(): Unit = {
    val b = new TypeConfusion.B
    val a1 = new TypeConfusion.A
    val o = b
    var a: TypeConfusion.A = null
    if (staticallyUnknown) a = a1
    else a = o.asInstanceOf[TypeConfusion.A]
    queryFor(a)
  }
}