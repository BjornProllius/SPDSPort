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
package boomerang

import boomerang.flowfunction.DefaultBackwardFlowFunction
import boomerang.flowfunction.DefaultForwardFlowFunction
import boomerang.flowfunction.IBackwardFlowFunction
import boomerang.flowfunction.IForwardFlowFunction
import boomerang.scene.AllocVal
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.stats.IBoomerangStats
import boomerang.stats.SimpleBoomerangStats
import com.google.common.base.Joiner
import java.lang.reflect.InvocationTargetException
import java.util
import java.util.Optional

class DefaultBoomerangOptions extends Nothing {
  def isAllocationVal(`val`: Nothing): Boolean = {
    if (!trackStrings && `val`.isStringBufferOrBuilder) return false
    if (trackNullAssignments && `val`.isNull) return true
    if ((getArrayStrategy ne ArrayStrategy.DISABLED) && `val`.isArrayAllocationVal) return true
    if (trackStrings && `val`.isStringConstant) return true
    if (!trackAnySubclassOfThrowable && `val`.isThrowableAllocationType) return false
    `val`.isNewExpr
  }

  @Override def getStaticFieldStrategy: Nothing = StaticFieldStrategy.SINGLETON

  @Override def getArrayStrategy: Nothing = ArrayStrategy.INDEX_SENSITIVE

  @Override def typeCheck = true

  @Override def trackReturnOfInstanceOf = false

  @Override def onTheFlyCallGraph = false

  @Override def throwFlows = false

  @Override def callSummaries = false

  @Override def fieldSummaries = false

  def trackAnySubclassOfThrowable = false

  def trackStrings = true

  def trackNullAssignments = true

  @Override def getAllocationVal(m: Nothing, stmt: Nothing, fact: Nothing): Nothing = {
    if (!stmt.isAssign) return Optional.empty
    if (!stmt.getLeftOp.equals(fact)) return Optional.empty
    if (isAllocationVal(stmt.getRightOp)) return Optional.of(new Nothing(stmt.getLeftOp, stmt, stmt.getRightOp))
    Optional.empty
  }

  @Override def analysisTimeoutMS = 10000

  @Override def statsFactory = new Nothing

  @Override def aliasing = true

  @Override def killNullAtCast = false

  @Override def trackStaticFieldAtEntryPointToClinit = false

  @Override def trackFields = true

  @Override def maxCallDepth: Int = -1

  @Override def maxUnbalancedCallDepth: Int = -1

  @Override def maxFieldDepth: Int = -1

  @Override def onTheFlyControlFlow = false

  @Override def toString: Nothing = {
    val cls = this.getClass
    val methodToVal = new Nothing
    val s = cls.getName
    import scala.collection.JavaConversions._
    for (m <- cls.getMethods) {
      val name = m.getName
      if (name.contains("toString")) continue //todo: continue is not supported
      if (m.getParameterCount eq 0) try {
        val `val` = m.invoke(this)
        methodToVal.add(name + "=" + `val`)
      } catch {
        case e@(_: Nothing | _: Nothing | _: Nothing) =>
      }
    }
    val joined = Joiner.on(",").join(methodToVal)
    "[" + s + "{" + joined + "}]"
  }

  @Override def ignoreInnerClassFields = false

  @Override def trackPathConditions = false

  @Override def prunePathConditions = false

  @Override def trackDataFlowPath = true

  @Override def trackImplicitFlows = false

  @Override def allowMultipleQueries = false

  def checkValid(): Unit = {
    if (trackPathConditions == false && prunePathConditions) throw new Nothing("InvalidCombinations of Options, Path Conditions must be ables when pruning path conditions")
  }

  @Override def handleMaps = true

  @Override def getForwardFlowFunctions = new Nothing(this)

  @Override def getBackwardFlowFunction = new Nothing(this)
}