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
package test.cases.fields.loops

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class LoopsWithFieldsIntraTest extends Nothing {
  @Test def oneFields(): Unit = {
    val x = new LoopsWithFieldsIntraTest#Node
    var p: LoopsWithFieldsIntraTest#Node = null
    while (staticallyUnknown) {
      if (staticallyUnknown) x.left = p
      else if (staticallyUnknown) x.right = p
      p = x
    }
    var t: LoopsWithFieldsIntraTest#Node = null
    if (staticallyUnknown) t = x.left
    else t = x.right
    val h = t
    queryFor(h)
  }

  @Test def twoFields(): Unit = {
    val x = new LoopsWithFieldsIntraTest#Node
    var p: LoopsWithFieldsIntraTest#Node = null
    while (staticallyUnknown) {
      if (staticallyUnknown) x.left.right = p
      else if (staticallyUnknown) x.right.left = p
      p = x
    }
    var t: LoopsWithFieldsIntraTest#Node = null
    if (staticallyUnknown) t = x.left.right
    else t = x.right.left
    val h = t
    queryFor(h)
  }

  @Test def twoFieldSimpleLoop(): Unit = {
    val x = new LoopsWithFieldsIntraTest#Node
    while (staticallyUnknown) x.left.right = x
    val h = x.left.right
    queryFor(h)
  }

  @Test def twoFieldSimpleLoopWithBranched(): Unit = {
    val x = new LoopsWithFieldsIntraTest#Node
    while (staticallyUnknown) if (staticallyUnknown) x.left.right = x
    else x.right.left = null
    val h = x.left.right
    queryFor(h)
  }

  @Test def threeFields(): Unit = {
    var x = new LoopsWithFieldsIntraTest#TreeNode
    var p: LoopsWithFieldsIntraTest#TreeNode = null
    while (staticallyUnknown) {
      if (staticallyUnknown) x.left.right = p
      else if (staticallyUnknown) x.right.left = p
      else {
        val u = x.parent
        x = u
      }
      p = x
    }
    var t: LoopsWithFieldsIntraTest#TreeNode = null
    if (staticallyUnknown) t = x.left.right
    else t = x.right.left
    val h = t
    queryFor(h)
  }

  private class Node extends Nothing {
    private[loops] val left = new LoopsWithFieldsIntraTest#Node
    private[loops] val right = new LoopsWithFieldsIntraTest#Node
  }

  private class TreeNode extends Nothing {
    private[loops] val left = new LoopsWithFieldsIntraTest#TreeNode
    private[loops] val right = new LoopsWithFieldsIntraTest#TreeNode
    private[loops] val parent = new LoopsWithFieldsIntraTest#TreeNode
  }

  private class SingleNode extends Nothing {
    private[loops] val left: LoopsWithFieldsIntraTest#SingleNode = null
  }

  @Test def oneFieldSimpleLoopSingle(): Unit = {
    val x = new LoopsWithFieldsIntraTest#SingleNode
    while (staticallyUnknown) x.left = x
    val h = x.left
    queryFor(h)
  }
}