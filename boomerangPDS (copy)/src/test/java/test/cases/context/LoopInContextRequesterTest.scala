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
package test.cases.context

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object LoopInContextRequesterTest {
  trait ILoop {
    def loop(): Unit
  }
}

class LoopInContextRequesterTest extends Nothing {
  @Test def loop(): Unit = {
    var c: LoopInContextRequesterTest.ILoop = null
    c = new LoopInContextRequesterTest#Loop1
    c.loop()
  }

  class Loop1 extends LoopInContextRequesterTest.ILoop {
    private[context] val a = new LoopInContextRequesterTest#A

    @Override override def loop(): Unit = {
      if (staticallyUnknown) loop()
      val x = a.d
      queryFor(x)
    }
  }

  class A {
    private[context] val d = new Nothing() {}
    private[context] val f = new LoopInContextRequesterTest#A
  }
}