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
package test.cases.fields.complexity

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object Recursion2LongTest {
  trait IFoo {
    def before(ds: Recursion2LongTest.DS): Recursion2LongTest.DS

    def after(ds: Recursion2LongTest.DS): Recursion2LongTest.DS
  }

  private class DS {
    var a: Recursion2LongTest.DS = null
    private val result: Nothing = null
  }

  class A extends Recursion2LongTest.IFoo {
    var foo: Recursion2LongTest.IFoo = null

    override def before(ds: Recursion2LongTest.DS): Recursion2LongTest.DS = {
      if (random) ds = ds.a
      if (random) ds.a = ds
      if (random) ds.a = null
      if (random) ds = foo.before(ds)
      ds
    }

    override def after(ds: Recursion2LongTest.DS): Recursion2LongTest.DS = {
      var ret = ds
      if (random) ret = foo.after(ds)
      if (random) ret = ds.a
      if (random) ret.a = ds
      if (random) ret.a = null
      ret
    }
  }

  def random = true
}

class Recursion2LongTest extends Nothing {
  @Test def test(): Unit = {
    val alloc = new Nothing
    val alias = main(alloc, new Recursion2LongTest.A)
    queryFor(alias)
  }

  def main(`object`: Nothing, foo: Recursion2LongTest.IFoo): Nothing = {
    var ds = new Recursion2LongTest.DS
    ds.result = `object`
    val a = foo.before(ds)
    val b = foo.after(a)
    ds = b
    // ds = foo.before(ds);
    // ds = foo.after(ds);
    // ds = foo.before(ds);
    // ds = foo.after(ds); // ...
    ds.result
  }
}