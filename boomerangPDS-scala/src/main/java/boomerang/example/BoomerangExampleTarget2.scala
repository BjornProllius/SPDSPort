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
package boomerang.example

object BoomerangExampleTarget2 {
  def main(args: Array[String]): Unit = {
    val a = new ClassWithField()
    val b = a
    val n = new NestedClassWithField()
    n.nested = b
    staticCallOnFile(n)
  }

  private def staticCallOnFile(n: NestedClassWithField): Unit = {
    println("Will print value 10")
    println(n.nested.field)
  }

  class ClassWithField {
    var field: Int = 10
  }

  class ObjectOfInterest {}

  class NestedClassWithField {
    var nested: ClassWithField = _
  }
}