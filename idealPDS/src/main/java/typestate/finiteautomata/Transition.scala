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

 class Transition(val from: State, val to: State, private val rep: String) extends ITransition {
 
   def this(from: State, to: State) = this(from, to, null)
 
   override def hashCode: Int = {
     val prime: Int = 31
     var result: Int = 1
     result = prime * result + (if (from == null) 0 else from.hashCode)
     result = prime * result + (if (rep == null) 0 else rep.hashCode)
     result = prime * result + (if (to == null) 0 else to.hashCode)
     result
   }
 
   override def equals(obj: Any): Boolean = {
     if (this == obj) return true
     if (obj == null) return false
     if (getClass != obj.getClass) return false
     val other: Transition = obj.asInstanceOf[Transition]
     if (from == null) {
       if (other.from != null) return false
     } else if (from != other.from) return false
     if (rep == null) {
       if (other.rep != null) return false
     } else if (rep != other.rep) return false
     if (to == null) {
       if (other.to != null) return false
     } else if (to != other.to) return false
     true
   }
 
   override def toString: String = if (rep != null) rep else s"$from -> $to"
 }
 
 object Transition {
   private var instance: Transition = _
 
   def identity(): Transition = {
     if (instance == null) instance = new Transition(null, null, "ID -> ID")
     instance
   }
 }
 