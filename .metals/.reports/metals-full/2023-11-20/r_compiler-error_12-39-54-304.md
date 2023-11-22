file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/idealPDS-scala/src/main/java/ideal/IDEALAnalysis.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/idealPDS-scala/src/main/java/ideal/IDEALAnalysis.java
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

import boomerang.ForwardQuery;
import boomerang.Query;
import boomerang.WeightedForwardQuery;
import boomerang.results.ForwardBoomerangResults;
import boomerang.scene.AnalysisScope;
import boomerang.scene.ControlFlowGraph.Edge;
import com.google.common.base.Stopwatch;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import typestate.TransitionFunction;
import wpds.impl.Weight;

public class IDEALAnalysis<W extends Weight> {

  private static final Logger LOGGER = LoggerFactory.getLogger(IDEALAnalysis.class);

  public static boolean PRINT_OPTIONS = false;

  protected final IDEALAnalysisDefinition<W> analysisDefinition;
  private final AnalysisScope seedFactory;
  private int seedCount;
  private Map<WeightedForwardQuery<W>, Stopwatch> analysisTime = new HashMap<>();
  private Set<WeightedForwardQuery<W>> timedoutSeeds = new HashSet<>();

  public IDEALAnalysis(final IDEALAnalysisDefinition<W> analysisDefinition) {
    this.analysisDefinition = analysisDefinition;
    this.seedFactory =
        new AnalysisScope(analysisDefinition.callGraph()) {

          @Override
          protected Collection<WeightedForwardQuery<W>> generate(Edge stmt) {
            return analysisDefinition.generate(stmt);
          }
        };
  }

  public void run() {
    printOptions();

    Collection<Query> initialSeeds = seedFactory.computeSeeds();

    if (initialSeeds.isEmpty()) LOGGER.info("No seeds found!");
    else LOGGER.info("Analysing {} seeds!", initialSeeds.size());
    for (Query s : initialSeeds) {
      if (!(s instanceof WeightedForwardQuery)) continue;
      WeightedForwardQuery<W> seed = (WeightedForwardQuery<W>) s;
      seedCount++;
      LOGGER.info("Analyzing {}", seed);
      Stopwatch watch = Stopwatch.createStarted();
      analysisTime.put(seed, watch);
      run(seed);
      watch.stop();
      LOGGER.debug(
          "Analyzed (finished,timedout): \t ({},{}) of {} seeds",
          (seedCount - timedoutSeeds.size()),
          timedoutSeeds.size(),
          initialSeeds.size());
    }
  }

  public ForwardBoomerangResults<W> run(ForwardQuery seed) {
    IDEALSeedSolver<W> idealAnalysis = new IDEALSeedSolver<W>(analysisDefinition, seed);
    ForwardBoomerangResults<W> res;
    try {
      res = idealAnalysis.run();
    } catch (IDEALSeedTimeout e) {
      res = (ForwardBoomerangResults<W>) e.getLastResults();
      timedoutSeeds.add((WeightedForwardQuery) seed);
    }
    analysisDefinition.getResultHandler().report((WeightedForwardQuery) seed, res);
    return res;
  }

  private void printOptions() {
    if (PRINT_OPTIONS) {
      System.out.println(analysisDefinition);
    }
  }

  public Collection<Query> computeSeeds() {
    return seedFactory.computeSeeds();
  }

  public Stopwatch getAnalysisTime(WeightedForwardQuery<TransitionFunction> key) {
    return analysisTime.get(key);
  }

  public boolean isTimedout(WeightedForwardQuery<TransitionFunction> key) {
    return timedoutSeeds.contains(key);
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
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:42)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:60)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:60)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:60)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:81)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:99)
```
#### Short summary: 

java.util.NoSuchElementException: next on empty iterator