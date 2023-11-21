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
  def main(args: Nothing*): Unit = {
    val a = new BoomerangExampleTarget2.ClassWithField
    val b = a
    val n = new BoomerangExampleTarget2.NestedClassWithField
    n.nested = b
    staticCallOnFile(n)
  }

  private def staticCallOnFile(n: BoomerangExampleTarget2.NestedClassWithField): Unit = {
    System.out.println("Will print value 10")
    System.out.println(n.nested.field)
  }

  class ClassWithField {
    var field = 10
  }

  class ObjectOfInterest {}

  class NestedClassWithField {
    var nested: BoomerangExampleTarget2.ClassWithField = null
  }
}