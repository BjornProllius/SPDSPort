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
package test.cases.sets

import java.util
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object CustomSetTest {
  class MySetIterator extends Nothing {
    private[sets] val content = new Array[Nothing](10)
    private[sets] val curr = 0

    private[sets] def add(o: Nothing): Unit = {
      content(o.hashCode) = o
    }

    @Override def hasNext: Boolean = curr < 10

    @Override def next: Nothing = content({
      curr += 1; curr - 1
    })

    @Override def remove(): Unit = {

      // TODO Auto-generated method stub
    }
  }
}

class CustomSetTest extends Nothing {
  @Test def mySetIteratorTest(): Unit = {
    val mySet = new CustomSetTest.MySetIterator
    val alias = new Nothing() {}
    mySet.add(alias)
    var query: Nothing = null
    while (mySet.hasNext) query = mySet.next
    queryFor(query)
  }

  @Test def mySetIterableTest(): Unit = {
    val mySet = new CustomSetTest#MySet
    val alias = new Nothing
    mySet.add(alias)
    var query: Nothing = null
    import scala.collection.JavaConversions._
    for (el <- mySet) {
      query = el
    }
    queryFor(query)
  }

  class MySet extends Nothing {
    private[sets] val content = new Array[Nothing](10)
    private[sets] val curr = 0
    private[sets] val it: Nothing = null

    private[sets] def add(o: Nothing): Unit = {
      content(o.hashCode) = o
    }

    @Override def iterator: Nothing = {
      if (it == null) it = new CustomSetTest#MySet#SetIterator
      it
    }

    private class SetIterator extends Nothing {
      @Override def hasNext: Boolean = curr < 10

      @Override def next: Nothing = content({
        curr += 1; curr - 1
      })

      @Override def remove(): Unit = {

        // TODO Auto-generated method stub
      }
    }
  }
}