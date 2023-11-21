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

import boomerang.callgraph.BoomerangResolver
import boomerang.callgraph.ICallerCalleeResolutionStrategy.Factory
import boomerang.flowfunction.IBackwardFlowFunction
import boomerang.flowfunction.IForwardFlowFunction
import boomerang.scene.AllocVal
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.stats.IBoomerangStats
import java.util.Optional

object BoomerangOptions {
  object StaticFieldStrategy extends Enumeration {
    type StaticFieldStrategy = Value
    val FLOW_SENSITIVE, SINGLETON, IGNORE = Value
  }

  object ArrayStrategy extends Enumeration {
    type ArrayStrategy = Value
    val DISABLED, INDEX_SENSITIVE, INDEX_INSENSITIVE = Value
  }
}

trait BoomerangOptions {
  def getResolutionStrategy: Nothing = BoomerangResolver.FACTORY

  def checkValid(): Unit

  def trackImplicitFlows: Boolean

  def handleMaps: Boolean

  def getForwardFlowFunctions: Nothing

  def getStaticFieldStrategy: BoomerangOptions.StaticFieldStrategy

  def getArrayStrategy: BoomerangOptions.ArrayStrategy

  def typeCheck: Boolean

  def onTheFlyCallGraph: Boolean

  def throwFlows: Boolean

  def callSummaries: Boolean

  def fieldSummaries: Boolean

  def analysisTimeoutMS: Int

  def getAllocationVal(m: Nothing, stmt: Nothing, fact: Nothing): Nothing

  def statsFactory: Nothing

  def aliasing: Boolean

  /**
   * Assume we propagate an object of soot.NullType in variable y and the propagation reaches a
   * statement x = (Object) y.
   *
   * @return If set to true, the propagation will NOT continue in x. This does not match the runtime
   *         semantics. At runtime, null can be cast to any RefType! Though a check (null instanceof
   *         Object) returns false.
   */
  def killNullAtCast: Boolean

  def trackReturnOfInstanceOf: Boolean

  def trackStaticFieldAtEntryPointToClinit: Boolean

  def trackFields: Boolean

  def maxFieldDepth: Int

  def maxCallDepth: Int

  def maxUnbalancedCallDepth: Int

  def onTheFlyControlFlow: Boolean

  def ignoreInnerClassFields: Boolean

  def trackPathConditions: Boolean

  def prunePathConditions: Boolean

  def trackDataFlowPath: Boolean

  def allowMultipleQueries: Boolean

  def getBackwardFlowFunction: Nothing
}