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
package test.cases.string

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object StringTest {
  private class T(private var string: Nothing) extends Nothing {
  }

  private class MyAbstractStringBuilder private[string]

  /** This no-arg constructor is necessary for serialization of subclasses. */ {
    /** The value is used for character storage. */
    private[string] var value: Array[Char] = null
    /** The count is the number of characters used. */
    private[string] var count = 0

    /** Creates an AbstractStringBuilder of the specified capacity. */
    def this(capacity: Int) {
      this()
      value = new Array[Char](capacity)
    }

    def append(str: Nothing): StringTest.MyAbstractStringBuilder = {
      // if (str == null)
      // return appendNull();
      val len = str.length
      // ensureCapacityInternal(count + len);
      str.getChars(0, len, value, count)
      count += len
      this
    }
  }

  private class MyStringBuilder(str: Nothing) extends StringTest.MyAbstractStringBuilder(str.length + 16) {
    append(str)

    @Override def toString: Nothing = {
      // Create a copy, don't share the array
      new Nothing(value, 0, count)
    }
  }
}

class StringTest extends Nothing {
  @Test def stringConcat(): Unit = {
    var query = "a" + "b"
    if (staticallyUnknown) query += "c"
    queryFor(query)
  }

  @Test def stringConcatQueryByPass(): Unit = {
    val t = new StringTest.T("a" + staticallyUnknown)
    queryFor(t)
  }

  @Test def stringbufferQueryByPass(): Unit = {
    val s = new Nothing
    s.append("")
    s.append("")
    s.append("")
    val t = new StringTest.T(s.toString)
    val t2 = new Nothing
    queryFor(t)
  }

  @Test def stringToCharArray(): Unit = {
    val s = "password".toCharArray
    queryFor(s)
  }

  @Test def stringBuilderTest(): Unit = {
    val b = new Nothing("Test")
    b.append("ABC")
    val s = b.toString
    queryFor(s)
  }

  @Test def stringBuilder1Test(): Unit = {
    val alloc = "Test"
    val b = new StringTest.MyStringBuilder(alloc)
    val s = b.toString
    queryFor(s)
  }
}