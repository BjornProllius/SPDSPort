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

import org.junit.Ignore
import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class FieldsBranchedTest extends Nothing {
  @Ignore
  @Test def twoFieldsNoLoop(): Unit = {
    val x = new FieldsBranchedTest#Node
    if (staticallyUnknown) x.left.right = x
    else if (staticallyUnknown) x.right.left = x
    var t: FieldsBranchedTest#Node = null
    if (staticallyUnknown) t = x.left.right
    else t = x.right.left
    val h = t
    queryFor(h)
  }

  @Test
  @Ignore def twoFieldsNoLoop2(): Unit = {
    val x = new FieldsBranchedTest#Node
    var t: FieldsBranchedTest#Node = null
    if (staticallyUnknown) {
      x.left.right = x
      t = x.left.right
    }
    else if (staticallyUnknown) {
      x.right.left = x
      t = x.right.left
    }
    val h = t
    queryFor(h)
  }

  @Test def oneFieldsNoLoop(): Unit = {
    val x = new FieldsBranchedTest#Node
    if (staticallyUnknown) x.left = x
    else if (staticallyUnknown) x.right = x
    var t: FieldsBranchedTest#Node = null
    if (staticallyUnknown) t = x.left
    else t = x.right
    val h = t
    queryFor(h)
  }

  private class Node extends Nothing {
    private[fields] val left = new FieldsBranchedTest#Node
    private[fields] val right = new FieldsBranchedTest#Node
  }
}