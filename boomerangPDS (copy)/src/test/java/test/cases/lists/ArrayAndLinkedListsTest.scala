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
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

class ArrayAndLinkedListsTest extends Nothing {
  @Override protected def includeJDK = true

  @Test def addAndRetrieve(): Unit = {
    val list1 = new Nothing
    val o = new Nothing
    add(list1, o)
    val o2 = new Nothing
    val list2 = new Nothing
    add(list2, o2)
    queryFor(o)
  }

  private def add(list1: Nothing, o: Nothing): Unit = {
    list1.add(o)
  }

  @Override protected def errorOnVisitMethod: Nothing = Collections.singleton("<java.util.ArrayList: boolean add(java.lang.Object)>")
}