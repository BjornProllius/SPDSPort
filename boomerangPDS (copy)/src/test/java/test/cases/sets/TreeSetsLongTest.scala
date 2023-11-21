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
import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

class TreeSetsLongTest extends Nothing {
  @Ignore
  @Test def addAndRetrieve(): Unit = {
    val set = new Nothing
    var alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    var alias2: Nothing = null
    import scala.collection.JavaConversions._
    for (o <- set) {
      alias2 = o
    }
    val ir = alias2
    val query2 = ir
    val set2 = new Nothing
    var alias1 = new Nothing
    set2.add(alias1)
    alias1 = new Nothing
    set2.add(alias1)
    alias1 = new Nothing
    queryFor(query2)
    otherMap()
    otherMap2()
    hashMap()
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
    var alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    var alias2: Nothing = null
    import scala.collection.JavaConversions._
    for (o <- set) {
      alias2 = o
    }
  }

  private def otherMap(): Unit = {
    val set = new Nothing
    var alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    alias = new Nothing
    set.add(alias)
    var alias2: Nothing = null
    import scala.collection.JavaConversions._
    for (o <- set) {
      alias2 = o
    }
  }

  @Override protected def includeJDK = true
}