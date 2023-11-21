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

class HashMapGetLongTest extends Nothing {
  @Test def addAndRetrieve(): Unit = {
    val map = new Nothing
    val key = new Nothing
    val alias3 = new Nothing
    map.put(key, alias3)
    val query = map.get(key)
    queryFor(query)
  }

  @Test def addAndLoadFromOther(): Unit = {
    val map = new Nothing
    val key = new Nothing
    val loadKey = new Nothing
    val alias3 = new Nothing
    map.put(key, alias3)
    val query = map.get(loadKey)
    queryFor(query)
  }

  @Override protected def includeJDK = true
}