file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/example/BoomerangExampleTarget1.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/example/BoomerangExampleTarget1.java
text:
```scala
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

object BoomerangExampleTarget1 {
  def main(args: Array[String]): Unit = {
    val a = new ClassWithField
    a.field = new ObjectOfInterest
    val b = a
    val n = new NestedClassWithField
    n.nested = b
    staticCallOnFile(a, n)
  }

  private def staticCallOnFile(x: ClassWithField, n: NestedClassWithField): Unit = {
    val queryVariable: ObjectOfInterest = x.field
    // The analysis triggers a query for the following variable
    queryFor(queryVariable)
  }

  private def queryFor(queryVariable: ObjectOfInterest): Unit = {}

  class ClassWithField {
    var field: ObjectOfInterest = _
  }

  class ObjectOfInterest

  class NestedClassWithField {
    var nested: ClassWithField = _
  }
}

// package boomerang.example;

// public class BoomerangExampleTarget1 {
//   public static void main(String... args) {
//     ClassWithField a = new ClassWithField();
//     a.field = new ObjectOfInterest();
//     ClassWithField b = a;
//     NestedClassWithField n = new NestedClassWithField();
//     n.nested = b;
//     staticCallOnFile(a, n);
//   }

//   private static void staticCallOnFile(ClassWithField x, NestedClassWithField n) {
//     ObjectOfInterest queryVariable = x.field;
//     // The analysis triggers a query for the following variable
//     queryFor(queryVariable);
//   }

//   private static void queryFor(ObjectOfInterest queryVariable) {}

//   public static class ClassWithField {
//     public ObjectOfInterest field;
//   }

//   public static class ObjectOfInterest {}

//   public static class NestedClassWithField {
//     public ClassWithField nested;
//   }
// }

```



#### Error stacktrace:

```
scala.collection.Iterator$$anon$19.next(Iterator.scala:973)
	scala.collection.Iterator$$anon$19.next(Iterator.scala:971)
	scala.collection.mutable.MutationTracker$CheckedIterator.next(MutationTracker.scala:76)
	scala.collection.IterableOps.head(Iterable.scala:222)
	scala.collection.IterableOps.head$(Iterable.scala:222)
	scala.collection.AbstractIterable.head(Iterable.scala:933)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:168)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:42)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:60)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:60)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:60)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:81)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:99)
```
#### Short summary: 

java.util.NoSuchElementException: next on empty iterator