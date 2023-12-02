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

import boomerang.callgraph.{BoomerangResolver, ICallerCalleeResolutionStrategy}
import boomerang.flowfunction.{IBackwardFlowFunction, IForwardFlowFunction}
import boomerang.scene.{AllocVal, Method, Statement, Val}
import boomerang.stats.IBoomerangStats

import scala.concurrent.duration.Duration

trait BoomerangOptions {

  def getResolutionStrategy: ICallerCalleeResolutionStrategy.Factory = BoomerangResolver.FACTORY

  def checkValid(): Unit

  def trackImplicitFlows(): Boolean

  def handleMaps(): Boolean

  def getForwardFlowFunctions(): IForwardFlowFunction

  enum StaticFieldStrategy {
    case FLOW_SENSITIVE, SINGLETON, IGNORE
  }

  def getStaticFieldStrategy(): StaticFieldStrategy

  enum ArrayStrategy {
    case DISABLED, INDEX_SENSITIVE, INDEX_INSENSITIVE
  }

  def getArrayStrategy(): ArrayStrategy

  def typeCheck(): Boolean

  def onTheFlyCallGraph(): Boolean

  def throwFlows(): Boolean

  def callSummaries(): Boolean

  def fieldSummaries(): Boolean

  def analysisTimeoutMS(): Duration

  def getAllocationVal(m: Method, stmt: Statement, fact: Val): Option[AllocVal]

  def statsFactory(): IBoomerangStats

  def aliasing(): Boolean

  def killNullAtCast(): Boolean

  def trackReturnOfInstanceOf(): Boolean

  def trackStaticFieldAtEntryPointToClinit(): Boolean

  def trackFields(): Boolean

  def maxFieldDepth(): Int

  def maxCallDepth(): Int

  def maxUnbalancedCallDepth(): Int

  def onTheFlyControlFlow(): Boolean

  def ignoreInnerClassFields(): Boolean

  def trackPathConditions(): Boolean

  def prunePathConditions(): Boolean

  def trackDataFlowPath(): Boolean

  def allowMultipleQueries(): Boolean

  def getBackwardFlowFunction(): IBackwardFlowFunction
}