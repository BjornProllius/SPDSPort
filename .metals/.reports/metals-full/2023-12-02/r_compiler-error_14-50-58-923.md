file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDSPort/idealPDS/src/main/java/ideal/IDEALSeedTimeout.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDSPort/idealPDS/src/main/java/ideal/IDEALSeedTimeout.java
text:
```scala
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
  package ideal

  import boomerang.{WeightedBoomerang, ForwardBoomerangResults}
  import wpds.impl.Weight
  
  /** Created by johannesspath on 01.12.17. */
  class IDEALSeedTimeout[W <: Weight](
      val solver: IDEALSeedSolver[W],
      val timedoutSolver: WeightedBoomerang[W],
      val lastResults: ForwardBoomerangResults[W]
  ) extends RuntimeException {
  
    override def toString: String = "IDEAL Seed TimeoutException \n"
  }
  
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