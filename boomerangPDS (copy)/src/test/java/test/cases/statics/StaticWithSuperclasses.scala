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
package test.cases.statics

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object StaticWithSuperclasses {
  private object List {
    private val elementData = new Nothing
  }

  private class List {
    def get: Nothing = List.elementData
  }

  private object MyList {
    private val elementData2 = new Nothing
  }

  private class MyList extends StaticWithSuperclasses.List {
    override def get: Nothing = MyList.elementData2
  }
}

class StaticWithSuperclasses extends Nothing {
  @Test def simple(): Unit = {
    val list = new StaticWithSuperclasses.List
    val o = list.get
    queryForAndNotEmpty(o)
  }

  @Test def supclass(): Unit = {
    val list = new StaticWithSuperclasses.MyList
    val o = list.get
    queryForAndNotEmpty(o)
  }
}