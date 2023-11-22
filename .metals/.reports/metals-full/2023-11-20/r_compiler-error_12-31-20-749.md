file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/util/AccessPath.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/util/AccessPath.java
text:
```scala
package boomerang.util;

import boomerang.scene.Field;
import boomerang.scene.Val;
import com.google.common.collect.Lists;
import java.util.Collection;
import java.util.Set;

public class AccessPath {
  private final Val val;
  private final Collection<Field> fieldChain;

  public AccessPath(Val value) {
    this.val = value;
    this.fieldChain = Lists.newArrayList();
  }

  public AccessPath(Val value, Field field) {
    this.val = value;
    this.fieldChain = Lists.newArrayList(field);
  }

  public AccessPath(Val value, Collection<Field> fields) {
    this.val = value;
    this.fieldChain = fields;
  }

  @Override
  public String toString() {
    return val.toString()
        + ""
        + (fieldChain.isEmpty() ? "" : fieldChain.toString())
        + (isOverApproximated() ? "*" : "");
  }

  public boolean isOverApproximated() {
    return fieldChain instanceof Set;
  }

  public Val getBase() {
    return this.val;
  }

  public Collection<Field> getFields() {
    return fieldChain;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((fieldChain == null) ? 0 : fieldChain.hashCode());
    result = prime * result + ((val == null) ? 0 : val.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    AccessPath other = (AccessPath) obj;
    if (fieldChain == null) {
      if (other.fieldChain != null) {
        return false;
      }
    } else if (!fieldChain.equals(other.fieldChain)) {
      return false;
    }
    if (val == null) {
      if (other.val != null) {
        return false;
      }
    } else if (!val.equals(other.val)) {
      return false;
    }
    return true;
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