/**
 * ******************************************************************************
 * Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors: Johannes Spaeth - initial API and implementation
 * ******************************************************************************
 */

 package typestate.finiteautomata

 import boomerang.scene.DeclaredMethod
 import java.util.regex.Pattern
 import org.slf4j.{Logger, LoggerFactory}
 
 class MatcherTransition(from: State, methodMatcher: String, param: Parameter, to: State, `type`: Type)
     extends Transition(from, to) {
 
   private val LOGGER: Logger = LoggerFactory.getLogger(classOf[MatcherTransition])
   private var negate: Boolean = false
 
   def this(from: State, methodMatcher: String, negate: Boolean, param: Parameter, to: State, `type`: Type) = {
     this(from, methodMatcher, param, to, `type`)
     this.negate = negate
   }
 
   def matches(declaredMethod: DeclaredMethod): Boolean = {
     val matches: Boolean = Pattern.matches(methodMatcher, declaredMethod.getSubSignature)
     if (matches)
       LOGGER.debug(s"Found matching transition at call site ${declaredMethod.getInvokeExpr} for $this")
     negate != matches
   }
 
   def getType: Type = `type`
 
   def getParam: Parameter = param
 
   override def toString: String = super.toString
 
   override def hashCode: Int = {
     val prime: Int = 31
     var result: Int = super.hashCode
     result = prime * result + (if (methodMatcher == null) 0 else methodMatcher.hashCode)
     result = prime * result + (if (negate) 1231 else 1237)
     result = prime * result + (if (param == null) 0 else param.hashCode)
     result = prime * result + (if (`type` == null) 0 else `type`.hashCode)
     result
   }
 
   override def equals(obj: Any): Boolean = {
     if (this == obj) return true
     if (!super.equals(obj)) return false
     if (getClass != obj.getClass) return false
     val other: MatcherTransition = obj.asInstanceOf[MatcherTransition]
     if (methodMatcher == null) {
       if (other.methodMatcher != null) return false
     } else if (methodMatcher != other.methodMatcher) return false
     if (negate != other.negate) return false
     if (param != other.param) return false
     if (`type` != other.`type`) return false
     true
   }
 }
 