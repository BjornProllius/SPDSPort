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
package test.cases.lists

import java.util
import java.util.Collections
import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object LinkedListsTypeLongTest {
  private class A extends LinkedListsTypeLongTest.I with Nothing {
    @Override override def bar(): Unit = {

      // TODO Auto-generated method stub
    }
  }

  private class B extends LinkedListsTypeLongTest.I {
    @Override override def bar(): Unit = {

      // TODO Auto-generated method stub
    }
  }

  private trait I {
    def bar(): Unit
  }
}

class LinkedListsTypeLongTest extends Nothing {
  @Override protected def includeJDK = true

  @Test def addAndRetrieveWithIteratorWithTyping(): Unit = {
    val list2 = new Nothing
    val b = new LinkedListsTypeLongTest.B
    list2.add(b)
    val list1 = new Nothing
    val alias = new LinkedListsTypeLongTest.A
    list1.add(alias)
    var alias2: LinkedListsTypeLongTest.I = null
    import scala.collection.JavaConversions._
    for (o <- list1) {
      alias2 = o
    }
    val ir = alias2
    val query2 = ir
    query2.bar()
    queryFor(query2)
  }

  @Override protected def errorOnVisitMethod: Nothing = Collections.singleton("<test.cases.lists.LinkedListsTypeLongTest$B: void bar()>")
}