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

import boomerang.flowfunction.{DefaultBackwardFlowFunction, DefaultForwardFlowFunction, IBackwardFlowFunction, IForwardFlowFunction}
import boomerang.scene.{AllocVal, Method, Statement, Val}
import boomerang.stats.{IBoomerangStats, SimpleBoomerangStats}
import com.google.common.base.Joiner

import scala.reflect.{InvocationTargetException}
import scala.collection.mutable.ListBuffer
import scala.util.Try

class DefaultBoomerangOptions extends BoomerangOptions {

  override def isAllocationVal(val: Val): Boolean = {
    if (!trackStrings() && val.isStringBufferOrBuilder()) {
      return false
    }
    if (trackNullAssignments() && val.isNull()) {
      return true
    }
    if (getArrayStrategy() != ArrayStrategy.DISABLED && val.isArrayAllocationVal()) {
      return true
    }
    if (trackStrings() && val.isStringConstant()) {
      return true
    }
    if (!trackAnySubclassOfThrowable() && val.isThrowableAllocationType()) {
      return false
    }

    val.isNewExpr()
  }

  override def getStaticFieldStrategy(): StaticFieldStrategy = {
    StaticFieldStrategy.SINGLETON
  }

  override def getArrayStrategy(): ArrayStrategy = {
    ArrayStrategy.INDEX_SENSITIVE
  }

  override def typeCheck(): Boolean = {
    true
  }

  override def trackReturnOfInstanceOf(): Boolean = {
    false
  }

  override def onTheFlyCallGraph(): Boolean = {
    false
  }

  override def throwFlows(): Boolean = {
    false
  }

  override def callSummaries(): Boolean = {
    false
  }

  override def fieldSummaries(): Boolean = {
    false
  }

  override def trackAnySubclassOfThrowable(): Boolean = {
    false
  }

  override def trackStrings(): Boolean = {
    true
  }

  override def trackNullAssignments(): Boolean = {
    true
  }

  override def getAllocationVal(m: Method, stmt: Statement, fact: Val): Option[AllocVal] = {
    if (!stmt.isAssign()) {
      return None
    }
    if (!stmt.getLeftOp().equals(fact)) {
      return None
    }
    if (isAllocationVal(stmt.getRightOp())) {
      return Some(new AllocVal(stmt.getLeftOp(), stmt, stmt.getRightOp()))
    }
    None
  }

  override def analysisTimeoutMS(): Int = {
    10000
  }

  override def statsFactory(): IBoomerangStats = {
    new SimpleBoomerangStats()
  }

  override def aliasing(): Boolean = {
    true
  }

  override def killNullAtCast(): Boolean = {
    false
  }

  override def trackStaticFieldAtEntryPointToClinit(): Boolean = {
    false
  }

  override def trackFields(): Boolean = {
    true
  }

  override def maxCallDepth(): Int = {
    -1
  }

  override def maxUnbalancedCallDepth(): Int = {
    -1
  }

  override def maxFieldDepth(): Int = {
    -1
  }

  override def onTheFlyControlFlow(): Boolean = {
    false
  }

  override def toString(): String = {
    val cls = this.getClass()
    val methodToVal = new ListBuffer[String]()
    val s = cls.getName()
    for (m <- cls.getMethods()) {
      val name = m.getName()
      if (name.contains("toString")) continue

      if (m.getParameterCount() == 0) {
        try {
          val val = m.invoke(this)
          methodToVal.add(name + "=" + val)
        } catch {
          case _: IllegalAccessException | _: IllegalArgumentException | _: InvocationTargetException =>
        }
      }
    }
    val joined = Joiner.on(",").join(methodToVal)
    "[" + s + "{" + joined + "}]"
  }

  override def ignoreInnerClassFields(): Boolean = {
    false
  }

  override def trackPathConditions(): Boolean = {
    false
  }

  override def prunePathConditions(): Boolean = {
    false
  }

  override def trackDataFlowPath(): Boolean = {
    true
  }

  override def trackImplicitFlows(): Boolean = {
    false
  }

  override def allowMultipleQueries(): Boolean = {
    false
  }

  def checkValid(): Unit = {
    if (!trackPathConditions() && prunePathConditions()) {
      throw new RuntimeException("InvalidCombinations of Options, Path Conditions must be ables when pruning path conditions")
    }
  }

  override def handleMaps(): Boolean = {
    true
  }

  override def getForwardFlowFunctions(): IForwardFlowFunction = {
    new DefaultForwardFlowFunction(this)
  }

  override def getBackwardFlowFunction(): IBackwardFlowFunction = {
    new DefaultBackwardFlowFunction(this)
  }
}
