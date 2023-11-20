file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/flowfunction/DefaultForwardFlowFunction.java
### java.util.NoSuchElementException: next on empty iterator

occurred in the presentation compiler.

action parameters:
offset: 779
uri: file:///C:/Users/tifav/OneDrive%20-%20ualberta.ca/Desktop/Uni/UofA/Year%204/Term%201/CMPUT%20416/SPDS%20Port/SPDSPort/boomerangPDS-scala/src/main/java/boomerang/flowfunction/DefaultForwardFlowFunction.java
text:
```scala
package boomerang.flowfunction;

import boomerang.BoomerangOptions;
import boomerang.ForwardQuery;
import boomerang.scene.ControlFlowGraph.Edge;
import boomerang.scene.Field;
import boomerang.scene.InvokeExpr;
import boomerang.scene.Method;
import boomerang.scene.Pair;
import boomerang.scene.Statement;
import boomerang.scene.StaticFieldVal;
import boomerang.scene.Val;
import boomerang.solver.ForwardBoomerangSolver;
import boomerang.solver.Strategies;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import sync.pds.solver.SyncPDSSolver.PDSSystem;
import sync.pds.solver.nodes.ExclusionNode;
import sync.pds.solver.n@@odes.Node;
import sync.pds.solver.nodes.NodeWithLocation;
import sync.pds.solver.nodes.PopNode;
import sync.pds.solver.nodes.PushNode;
import wpds.interfaces.State;

public class DefaultForwardFlowFunction implements IForwardFlowFunction {

  private final BoomerangOptions options;
  private Strategies strategies;
  private ForwardBoomerangSolver solver;

  public DefaultForwardFlowFunction(BoomerangOptions opts) {
    this.options = opts;
  }

  @Override
  public Set<Val> returnFlow(Method method, Statement curr, Val value) {
    Set<Val> out = Sets.newHashSet();
    if (curr.isThrowStmt() && !options.throwFlows()) {
      return Collections.emptySet();
    }
    if (curr.isReturnStmt()) {
      if (curr.getReturnOp().equals(value)) {
        out.add(value);
      }
    }
    if (!method.isStatic()) {
      if (method.getThisLocal().equals(value)) {
        out.add(value);
      }
    }
    for (Val param : method.getParameterLocals()) {
      if (param.equals(value)) {
        out.add(value);
      }
    }
    if (value.isStatic()) {
      out.add(value);
    }
    return out;
  }

  @Override
  public Set<Val> callFlow(Statement callSite, Val fact, Method callee) {
    if (!callSite.containsInvokeExpr()) {
      throw new RuntimeException("Call site does not contain an invoke expression.");
    }
    if (callee.isStaticInitializer()) {
      return Collections.emptySet();
    }
    Set<Val> out = Sets.newHashSet();
    InvokeExpr invokeExpr = callSite.getInvokeExpr();
    if (invokeExpr.isInstanceInvokeExpr()) {
      if (invokeExpr.getBase().equals(fact) && !callee.isStatic()) {
        out.add(callee.getThisLocal());
      }
    }
    int i = 0;
    List<Val> parameterLocals = callee.getParameterLocals();
    for (Val arg : invokeExpr.getArgs()) {
      if (arg.equals(fact) && parameterLocals.size() > i) {
        out.add(parameterLocals.get(i));
      }
      i++;
    }
    if (fact.isStatic()) {
      out.add(fact.withNewMethod(callee));
    }
    return out;
  }

  @Override
  public Set<State> normalFlow(ForwardQuery query, Edge nextEdge, Val fact) {
    Statement succ = nextEdge.getStart();
    Set<State> out = Sets.newHashSet();
    if (killFlow(succ, fact)) {
      return out;
    }
    if (!succ.isFieldWriteWithBase(fact)) {
      // always maintain data-flow if not a field write // killFlow has
      // been taken care of
      if (!options.trackReturnOfInstanceOf()
 !(query.getType().isNullType() && succ.isInstanceOfStatement(fact))) {
        out.add(new Node<>(nextEdge, fact));
      }
    } else {
      out.add(new ExclusionNode<>(nextEdge, fact, succ.getWrittenField()));
    }
    if (succ.isAssign()) {
      Val leftOp = succ.getLeftOp();
      Val rightOp = succ.getRightOp();
      if (rightOp.equals(fact)) {
        if (succ.isFieldStore()) {
          Pair<Val, Field> ifr = succ.getFieldStore();
          if (options.trackFields()) {
            if (!options.ignoreInnerClassFields() || !ifr.getY().isInnerClassField()) {
              out.add(new PushNode<>(nextEdge, ifr.getX(), ifr.getY(), PDSSystem.FIELDS));
            }
          }
        } else if (succ.isStaticFieldStore()) {
          StaticFieldVal sf = succ.getStaticField();
          if (options.trackFields()) {
            strategies.getStaticFieldStrategy().handleForward(nextEdge, rightOp, sf, out);
          }
        } else if (leftOp.isArrayRef()) {
          Pair<Val, Integer> arrayBase = succ.getArrayBase();
          if (options.trackFields()) {
            strategies.getArrayHandlingStrategy().handleForward(nextEdge, arrayBase, out);
          }
        } else {
          out.add(new Node<>(nextEdge, leftOp));
        }
      }
      if (succ.isFieldLoad()) {
        Pair<Val, Field> ifr = succ.getFieldLoad();
        if (ifr.getX().equals(fact)) {
          NodeWithLocation<Edge, Val, Field> succNode =
              new NodeWithLocation<>(nextEdge, leftOp, ifr.getY());
          out.add(new PopNode<>(succNode, PDSSystem.FIELDS));
        }
      } else if (succ.isStaticFieldLoad()) {
        StaticFieldVal sf = succ.getStaticField();
        if (fact.isStatic() && fact.equals(sf)) {
          out.add(new Node<>(nextEdge, leftOp));
        }
      } else if (rightOp.isArrayRef()) {
        Pair<Val, Integer> arrayBase = succ.getArrayBase();
        if (arrayBase.getX().equals(fact)) {
          NodeWithLocation<Edge, Val, Field> succNode =
              new NodeWithLocation<>(nextEdge, leftOp, Field.array(arrayBase.getY()));
          out.add(new PopNode<>(succNode, PDSSystem.FIELDS));
        }
      } else if (rightOp.isCast()) {
        if (rightOp.getCastOp().equals(fact)) {
          out.add(new Node<>(nextEdge, leftOp));
        }
      } else if (rightOp.isInstanceOfExpr()
          && query.getType().isNullType()
          && options.trackReturnOfInstanceOf()) {
        if (rightOp.getInstanceOfOp().equals(fact)) {
          out.add(new Node<>(nextEdge, fact.withSecondVal(leftOp)));
        }
      } else if (succ.isPhiStatement()) {
        Collection<Val> phiVals = succ.getPhiVals();
        if (phiVals.contains(fact)) {
          out.add(new Node<>(nextEdge, succ.getLeftOp()));
        }
      }
    }

    return out;
  }

  protected boolean killFlow(Statement curr, Val value) {
    if (curr.isThrowStmt() || curr.isCatchStmt()) {
      return true;
    }

    if (curr.isAssign()) {
      // Kill x at any statement x = * during propagation.
      if (curr.getLeftOp().equals(value)) {
        // But not for a statement x = x.f
        if (curr.isFieldLoad()) {
          Pair<Val, Field> ifr = curr.getFieldLoad();
          if (ifr.getX().equals(value)) {
            return false;
          }
        }
        return true;
      }
      if (curr.isStaticFieldStore()) {
        StaticFieldVal sf = curr.getStaticField();
        if (value.isStatic() && value.equals(sf)) {
          return true;
        }
      }
    }
    return false;
  }

  @Override
  public Collection<State> callToReturnFlow(ForwardQuery query, Edge edge, Val fact) {
    if (FlowFunctionUtils.isSystemArrayCopy(edge.getStart().getInvokeExpr().getMethod())) {
      return systemArrayCopyFlow(edge, fact);
    }
    return normalFlow(query, edge, fact);
  }

  protected Collection<State> systemArrayCopyFlow(Edge edge, Val value) {
    Statement callSite = edge.getStart();
    if (value.equals(callSite.getInvokeExpr().getArg(0))) {
      Val arg = callSite.getInvokeExpr().getArg(2);
      return Collections.singleton(new Node<>(edge, arg));
    }
    return Collections.emptySet();
  }

  @Override
  public void setSolver(
      ForwardBoomerangSolver solver,
      Multimap<Field, Statement> fieldLoadStatements,
      Multimap<Field, Statement> fieldStoreStatements) {
    this.solver = solver;
    this.strategies = new Strategies<>(options, solver, fieldLoadStatements, fieldStoreStatements);
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