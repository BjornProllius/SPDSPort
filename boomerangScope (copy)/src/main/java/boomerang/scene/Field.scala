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
package boomerang.scene

import com.google.common.base.Objects
import wpds.interfaces.Empty
import wpds.interfaces.Location
import wpds.wildcard.ExclusionWildcard
import wpds.wildcard.Wildcard

object Field {
  def wildcard = new Field.WildcardField

  def empty = new Field.EmptyField("{}")

  private class EmptyField(rep: Nothing) extends Field(rep) with Nothing {
  }

  def string(key: Nothing) = new Field(key)

  def epsilon = new Field.EmptyField("eps_f")

  def array(index: Int): Field = {
    if (index == -1) return new Field.ArrayField
    new Field.ArrayField(index)
  }

  private class WildcardField extends Field("*") with Nothing {
  }

  private class ExclusionWildcardField(private val excludes: Field) extends Field with Nothing {
    @Override def excludes: Field = excludes

    @Override override def toString: Nothing = "not " + excludes

    @Override override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (excludes == null) 0
      else excludes.hashCode)
      result
    }

    @Override override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[Field.ExclusionWildcardField]
      if (excludes == null) if (other.excludes != null) return false
      else if (!(excludes == other.excludes)) return false
      true
    }
  }

  class ArrayField extends Field("array") {
    private[scene] val index = -1

    def this(index: Int) {
      this()
      super ("array")
      if (index < 0) throw new Nothing("Illegal Array field construction")
      this.index = index
    }

    @Override override def equals(o: Nothing): Boolean = {
      if (this eq o) return true
      if (o == null || (getClass ne o.getClass)) return false
      if (!super.equals(o)) return false
      val that = o.asInstanceOf[Field.ArrayField]
      index == that.index
    }

    @Override override def toString: Nothing = super.toString + (if (index == -1) " ANY_INDEX"
    else "Index: " + index)

    @Override override def hashCode: Int = Objects.hashCode(super.hashCode, index)

    @Override override def accepts(other: Nothing): Boolean = {
      if (this == other) return true
      index == -1 && other.isInstanceOf[Field.ArrayField]
    }

    def getIndex: Int = index
  }

  def exclusionWildcard(exclusion: Field) = new Field.ExclusionWildcardField(exclusion)
}

class Field protected extends Nothing {
  this.rep = null
  final private var rep: Nothing = null

  def this(rep: Nothing) {
    this()
    this.rep = rep
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[Field]
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    true
  }

  @Override def toString: Nothing = rep

  @Override def accepts(other: Nothing): Boolean = this == other

  def isInnerClassField = false
}