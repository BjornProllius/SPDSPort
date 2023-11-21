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

import boomerang.BoomerangOptions
import boomerang.DefaultBoomerangOptions
import java.util
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

class TreeMapMultipleInstancesTest extends Nothing {
  @Test def addAndRetrieve(): Unit = {
    val set = new Nothing
    val alias = new Nothing
    set.put(1, alias)
    val query2 = set.get(2)
    queryFor(query2)
    otherMap()
    otherMap2()
    hashMap()
  }

  @Test def contextSensitive(): Unit = {
    val map = new Nothing
    val alias = new Nothing
    val ret = addToMap(map, alias)
    val map2 = new Nothing
    val noAlias = new Nothing
    val ret2 = addToMap(map2, noAlias)
    System.out.println(ret2)
    queryFor(ret)
  }

  private def addToMap(map: Nothing, alias: Nothing) = {
    map.put(1, alias)
    val query2 = map.get(2)
    query2
  }

  private def hashMap(): Unit = {
    var map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
    map = new Nothing
    map.add(new Nothing)
  }

  private def otherMap2(): Unit = {
    val set = new Nothing
    val alias = new Nothing
    set.put(1, alias)
    set.put(2, alias)
    set.get(3)
  }

  private def otherMap(): Unit = {
    val set = new Nothing
    val alias = new Nothing
    set.put(1, alias)
    set.put(2, alias)
    set.get(3)
  }

  @Override protected def includeJDK = true

  @Override protected def createBoomerangOptions: Nothing = new Nothing() {
    @Override def handleMaps: Boolean = return false

    @Override def analysisTimeoutMS: Int = return analysisTimeout
  }
}