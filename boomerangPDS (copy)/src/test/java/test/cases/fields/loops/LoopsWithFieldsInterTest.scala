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

class LoopsWithFieldsInterTest extends Nothing {
  @Test def twoFields(): Unit = {
    val x = new LoopsWithFieldsInterTest#Node
    var p: LoopsWithFieldsInterTest#Node = null
    while (staticallyUnknown) {
      if (staticallyUnknown) leftOf(x).right = p
      else rightOf(x).left = p
      p = x
    }
    var t: LoopsWithFieldsInterTest#Node = null
    if (staticallyUnknown) t = rightOf(leftOf(x))
    else t = leftOf(rightOf(x))
    val h = t
    queryFor(h)
  }

  private def leftOf(x: LoopsWithFieldsInterTest#Node) = if (x == null) x.left
  else null

  private def rightOf(x: LoopsWithFieldsInterTest#Node) = if (x == null) x.right
  else null

  private def leftOf(x: LoopsWithFieldsInterTest#TreeNode) = if (x == null) x.left
  else null

  private def rightOf(x: LoopsWithFieldsInterTest#TreeNode) = if (x == null) x.left
  else null

  private def parentOf(x: LoopsWithFieldsInterTest#TreeNode) = if (x == null) x.parent
  else null

  @Test def threeFields(): Unit = {
    var x = new LoopsWithFieldsInterTest#TreeNode
    var p: LoopsWithFieldsInterTest#TreeNode = null
    while (staticallyUnknown) {
      if (staticallyUnknown) leftOf(x).right = p
      else if (staticallyUnknown) rightOf(x).left = p
      else {
        val u = parentOf(x)
        x = u
      }
      p = x
    }
    var t: LoopsWithFieldsInterTest#TreeNode = null
    if (staticallyUnknown) t = rightOf(leftOf(x))
    else t = leftOf(rightOf(x))
    val h = t
    queryFor(h)
  }

  private class Node extends Nothing {
    private[loops] val left = new LoopsWithFieldsInterTest#Node
    private[loops] val right = new LoopsWithFieldsInterTest#Node
  }

  private class TreeNode extends Nothing {
    private[loops] val left = new LoopsWithFieldsInterTest#TreeNode
    private[loops] val right = new LoopsWithFieldsInterTest#TreeNode
    private[loops] val parent = new LoopsWithFieldsInterTest#TreeNode
  }
}