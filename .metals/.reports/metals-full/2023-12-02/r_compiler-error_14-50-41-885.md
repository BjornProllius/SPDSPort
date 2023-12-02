file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDSPort/idealPDS/src/main/java/ideal/IDEALSeedTimeout.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
offset: 682
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDSPort/idealPDS/src/main/java/ideal/IDEALSeedTimeout.java
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
package ideal;

import boomerang.WeightedBoomerang;
import boomerang.results.ForwardBoomerangResults;
import wpds.impl.Weight;

/** Created by j@@ohannesspath on 01.12.17. */
public class IDEALSeedTimeout extends RuntimeException {
  private final IDEALSeedSolver<? extends Weight> solver;
  private WeightedBoomerang<? extends Weight> timedoutSolver;
  private ForwardBoomerangResults<? extends Weight> lastResults;

  public <W extends Weight> IDEALSeedTimeout(
      IDEALSeedSolver<W> solver,
      WeightedBoomerang<W> timedoutSolver,
      ForwardBoomerangResults<W> lastResults) {
    this.solver = solver;
    this.timedoutSolver = timedoutSolver;
    this.lastResults = lastResults;
  }

  public IDEALSeedSolver<? extends Weight> getSolver() {
    return solver;
  }

  public WeightedBoomerang<? extends Weight> getTimedoutSolver() {
    return timedoutSolver;
  }

  public ForwardBoomerangResults<? extends Weight> getLastResults() {
    return lastResults;
  }

  @Override
  public String toString() {
    return "IDEAL Seed TimeoutException \n";
  }
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
	scala.meta.internal.pc.HoverProvider$.hover(HoverProvider.scala:34)
	scala.meta.internal.pc.ScalaPresentationCompiler.hover$$anonfun$1(ScalaPresentationCompiler.scala:329)
```
#### Short summary: 

java.util.NoSuchElementException: next on empty iterator