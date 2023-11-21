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
package test.cases.fields

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class SummaryTest extends Nothing {
  @Test def branchedSummaryReuse(): Unit = {
    val x = new SummaryTest#A
    var query: SummaryTest#B = null
    if (staticallyUnknown) {
      x.f = new SummaryTest#B
      query = load(x)
    }
    else {
      x.f = new SummaryTest#B
      query = load(x)
    }
    queryFor(query)
  }

  @Test def simpleNoReuse(): Unit = {
    val x = new SummaryTest#A
    x.f = new SummaryTest#B
    val query = load(x)
    queryFor(query)
  }

  private def load(x: SummaryTest#A) = x.f

  private class A {
    private[fields] val f: SummaryTest#B = null
  }

  private class B extends Nothing {}
}