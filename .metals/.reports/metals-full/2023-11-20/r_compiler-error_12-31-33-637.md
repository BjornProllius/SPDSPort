file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/BoomerangOptions.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/BoomerangOptions.java
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
package boomerang;

import boomerang.callgraph.BoomerangResolver;
import boomerang.callgraph.ICallerCalleeResolutionStrategy.Factory;
import boomerang.flowfunction.IBackwardFlowFunction;
import boomerang.flowfunction.IForwardFlowFunction;
import boomerang.scene.AllocVal;
import boomerang.scene.Method;
import boomerang.scene.Statement;
import boomerang.scene.Val;
import boomerang.stats.IBoomerangStats;
import java.util.Optional;

public interface BoomerangOptions {

  default Factory getResolutionStrategy() {
    return BoomerangResolver.FACTORY;
  }

  void checkValid();

  boolean trackImplicitFlows();

  boolean handleMaps();

  IForwardFlowFunction getForwardFlowFunctions();

  enum StaticFieldStrategy {
    FLOW_SENSITIVE,
    SINGLETON,
    IGNORE
  }

  StaticFieldStrategy getStaticFieldStrategy();

  enum ArrayStrategy {
    DISABLED,
    INDEX_SENSITIVE,
    INDEX_INSENSITIVE
  }

  ArrayStrategy getArrayStrategy();

  boolean typeCheck();

  boolean onTheFlyCallGraph();

  boolean throwFlows();

  boolean callSummaries();

  boolean fieldSummaries();

  int analysisTimeoutMS();

  Optional<AllocVal> getAllocationVal(Method m, Statement stmt, Val fact);

  IBoomerangStats statsFactory();

  boolean aliasing();

  /**
   * Assume we propagate an object of soot.NullType in variable y and the propagation reaches a
   * statement x = (Object) y.
   *
   * @return If set to true, the propagation will NOT continue in x. This does not match the runtime
   *     semantics. At runtime, null can be cast to any RefType! Though a check (null instanceof
   *     Object) returns false.
   */
  boolean killNullAtCast();

  boolean trackReturnOfInstanceOf();

  boolean trackStaticFieldAtEntryPointToClinit();

  boolean trackFields();

  int maxFieldDepth();

  int maxCallDepth();

  int maxUnbalancedCallDepth();

  boolean onTheFlyControlFlow();

  boolean ignoreInnerClassFields();

  boolean trackPathConditions();

  boolean prunePathConditions();

  boolean trackDataFlowPath();

  boolean allowMultipleQueries();

  IBackwardFlowFunction getBackwardFlowFunction();
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