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
package typestate.finiteautomata

import boomerang.scene.DeclaredMethod
import java.util.regex.Pattern
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object MatcherTransition {
  private val LOGGER = LoggerFactory.getLogger(classOf[MatcherTransition])

  object Type extends Enumeration {
    type Type = Value
    val OnCall, None, OnCallToReturn, OnCallOrOnCallToReturn = Value
  }

  object Parameter extends Enumeration {
    type Parameter = Value
    val This, Param1, Param2 = Value
  }
}

class MatcherTransition extends Nothing {
  private var `type`: MatcherTransition.Type = null
  private var param: MatcherTransition.Parameter = null
  private var methodMatcher: Nothing = null
  private var negate = false

  def this(from: Nothing, methodMatcher: Nothing, param: MatcherTransition.Parameter, to: Nothing, `type`: MatcherTransition.Type) {
    this()
    super (from, to)
    this.methodMatcher = methodMatcher
    this.`type` = `type`
    this.param = param
  }

  def this(from: Nothing, methodMatcher: Nothing, negate: Boolean, param: MatcherTransition.Parameter, to: Nothing, `type`: MatcherTransition.Type) {
    this()
    super (from, to)
    this.methodMatcher = methodMatcher
    this.negate = negate
    this.`type` = `type`
    this.param = param
  }

  def matches(declaredMethod: Nothing): Boolean = {
    val matches = Pattern.matches(methodMatcher, declaredMethod.getSubSignature)
    if (matches) MatcherTransition.LOGGER.debug("Found matching transition at call site {} for {}", declaredMethod.getInvokeExpr, this)
    if (negate) !matches
    else matches
  }

  def getType: MatcherTransition.Type = `type`

  def getParam: MatcherTransition.Parameter = param

  @Override def toString: Nothing = super.toString

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (methodMatcher == null) 0
    else methodMatcher.hashCode)
    result = prime * result + (if (negate) 1231
    else 1237)
    result = prime * result + (if (param == null) 0
    else param.hashCode)
    result = prime * result + (if (`type` == null) 0
    else `type`.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[MatcherTransition]
    if (methodMatcher == null) if (other.methodMatcher != null) return false
    else if (!methodMatcher.equals(other.methodMatcher)) return false
    if (negate != other.negate) return false
    if (param ne other.param) return false
    if (`type` ne other.`type`) return false
    true
  }
}