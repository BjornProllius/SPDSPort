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
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class HashSetsLongTest extends Nothing {
  @Ignore
  @Test def addAndRetrieve(): Unit = {
    val set = new Nothing
    val alias = new Nothing() {}
    val alias3 = new Nothing() {}
    set.add(alias)
    set.add(alias3)
    var alias2: Nothing = null
    import scala.collection.JavaConversions._
    for (o <- set) {
      alias2 = o
    }
    val ir = alias2
    val query2 = ir
    queryFor(query2)
  }

  @Override protected def includeJDK = true
}