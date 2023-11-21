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

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object CustomMapTest {
  class Map {
    var m = new CustomMapTest.InnerMap

    def add(o: Nothing): Unit = {
      val map = this.m
      map.innerAdd(o)
      val alias = this.m
      val retrieved = alias.content
    }

    def get: Nothing = {
      val map = this.m
      map.get
    }
  }

  class InnerMap {
    var content: Nothing = null

    def innerAdd(o: Nothing): Unit = {
      content = o
    }

    def get: Nothing = content
  }
}

class CustomMapTest extends Nothing {
  @Test def storeAndLoad(): Unit = {
    val alloc = new Nothing
    val map = new CustomMapTest.Map
    map.add(alloc)
    val alias = map.get
    queryFor(alias)
  }

  @Test def storeAndLoadSimple(): Unit = {
    val alloc = new Nothing
    val map = new CustomMapTest.Map
    map.add(alloc)
    val alias = map.m.content
    queryFor(alias)
  }

  @Test def onlyInnerMapSimple(): Unit = {
    val alloc = new Nothing
    val map = new CustomMapTest.InnerMap
    map.innerAdd(alloc)
    val alias = map.content
    queryFor(alias)
  }

  @Test def storeAndLoadSimpleNoInnerClasses(): Unit = {
    val alloc = new Nothing
    val map = new Nothing
    map.add(alloc)
    val load = map.m
    val alias = load.content
    queryFor(alias)
  }
}