/**
 * ***************************************************************************** Copyright (c) 2020
 * CodeShield GmbH, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang.scene.wala

import boomerang.scene.DeclaredMethod
import boomerang.scene.InvokeExpr
import boomerang.scene.Val
import com.google.common.collect.Lists
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import java.util

class WALAInvokeExpr(private var inv: Nothing, private var m: Nothing) extends Nothing {
  private var argsCache: Nothing = null

  @Override def getArg(index: Int): Nothing = getArgs.get(index)

  @Override def getArgs: Nothing = {
    if (argsCache == null) {
      argsCache = Lists.newArrayList
      var i = if (inv.isStatic) 0
      else 1
      while (i < inv.getNumberOfPositionalParameters) {
        argsCache.add(new Nothing(inv.getUse(i), m))
        i += 1
      }
    }
    argsCache
  }

  @Override def isInstanceInvokeExpr: Boolean = inv.isDispatch || inv.isSpecial

  @Override def getBase = new Nothing(inv.getReceiver, m)

  @Override def getMethod = new Nothing(this, inv.getCallSite.getDeclaredTarget)

  @Override def isSpecialInvokeExpr: Boolean = inv.isSpecial

  @Override def isStaticInvokeExpr: Boolean = inv.isStatic
}