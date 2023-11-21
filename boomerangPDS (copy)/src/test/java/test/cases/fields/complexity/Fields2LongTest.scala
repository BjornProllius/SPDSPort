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
package test.cases.fields.complexity

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class Fields2LongTest extends Nothing {
  @Test def test(): Unit = {
    val x = new Fields2LongTest#TreeNode
    var p: Fields2LongTest#TreeNode = null
    while (staticallyUnknown) {
      if (staticallyUnknown) x.a = p
      if (staticallyUnknown) x.b = p
      p = x
    }
    var t: Fields2LongTest#TreeNode = null
    if (staticallyUnknown) t = x.a
    if (staticallyUnknown) t = x.b
    val h = t
    queryFor(h)
  }

  class TreeNode extends Nothing {
    private[complexity] val a = new Fields2LongTest#TreeNode
    private[complexity] val b = new Fields2LongTest#TreeNode
  }
}