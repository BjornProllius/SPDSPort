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

import java.util
import java.util.Collections
import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object InterfaceInvocation {
  private trait I {
    def bar(): Unit
  }

  private class A extends InterfaceInvocation.I with Nothing {
    @Override override def bar(): Unit = {
    }
  }

  private class B extends InterfaceInvocation.I {
    @Override override def bar(): Unit = {
    }
  }
}

class InterfaceInvocation extends Nothing {
  @Test def invokesInterface(): Unit = {
    val b = new InterfaceInvocation.B
    wrappedFoo(b)
    val a1 = new InterfaceInvocation.A
    val a2 = new InterfaceInvocation.A
    var a: InterfaceInvocation.A = null
    if (staticallyUnknown) a = a1
    else a = a2
    wrappedFoo(a)
    queryFor(a)
  }

  private def wrappedFoo(a: InterfaceInvocation.I): Unit = {
    foo(a)
  }

  private def foo(a: InterfaceInvocation.I): Unit = {
    a.bar()
  }

  @Override protected def errorOnVisitMethod: Nothing = Collections.singleton("<test.cases.typing.InterfaceInvocation$B: void bar()>")
}