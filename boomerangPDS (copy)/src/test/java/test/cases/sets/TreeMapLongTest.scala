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

class TreeMapLongTest extends Nothing {
  @Test def addAndRetrieve(): Unit = {
    val set = new Nothing
    val alias = new Nothing
    set.put(1, alias)
    val query2 = set.get(2)
    queryFor(query2)
  }

  @Override protected def includeJDK = true
}