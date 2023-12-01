/*******************************************************************************
 * Copyright (c) 2018 Fraunhofer IEM, Paderborn, Germany.
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *  
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     Johannes Spaeth - initial API and implementation
 *******************************************************************************/
package boomerang;

import java.util.Collection;
import java.util.Collections;

import boomerang.callgraph.BoomerangICFG;
import boomerang.callgraph.ObservableICFG;
import boomerang.callgraph.ObservableStaticICFG;
import boomerang.jimple.AllocVal;
import boomerang.jimple.Statement;
import boomerang.jimple.Val;
import boomerang.results.BackwardBoomerangResults;
import boomerang.seedfactory.SeedFactory;
import soot.SootMethod;
import soot.Unit;
import soot.jimple.AssignStmt;
import soot.jimple.Stmt;
import soot.jimple.toolkits.ide.icfg.JimpleBasedInterproceduralCFG;
import sync.pds.solver.nodes.Node;
import wpds.impl.Weight;

public abstract class WholeProgramBoomerang<W extends Weight> extends WeightedBoomerang<W> {
    private int reachableMethodCount;
    private int allocationSites;
    private SeedFactory<W> seedFactory;

    public WholeProgramBoomerang(BoomerangOptions opts) {
        super(opts);
    }

    public WholeProgramBoomerang() {
        this(new DefaultBoomerangOptions());
    }

    @Override
    public SeedFactory<W> getSeedFactory() {
        if (seedFactory == null) {
            seedFactory = new SeedFactory<W>() {

                @Override
                protected Collection<? extends Query> generate(SootMethod method, Stmt u) {
                    if (u instanceof AssignStmt) {
                        AssignStmt assignStmt = (AssignStmt) u;
                        if (options.isAllocationVal(assignStmt.getRightOp())) {
                            return Collections.singleton(
                                    new ForwardQuery(new Statement(u, method), new AllocVal(assignStmt.getLeftOp(),
                                            method, assignStmt.getRightOp(), new Statement((Stmt) u, method))));
                        }
                    }
                    return Collections.emptySet();
                }

                @Override
                public ObservableICFG<Unit, SootMethod> icfg() {
                    return new ObservableStaticICFG(new BoomerangICFG(false));
                }
            };
        }
        return seedFactory;
    }

    public void wholeProgramAnalysis() {
        long before = System.currentTimeMillis();

        for (Query s : getSeedFactory().computeSeeds()) {
            solve((ForwardQuery) s);
        }

        long after = System.currentTimeMillis();
        System.out.println("Analysis Time (in ms):\t" + (after - before));
        System.out.println("Analyzed methods:\t" + reachableMethodCount);
        System.out.println("Total solvers:\t" + this.getSolvers().size());
        System.out.println("Allocation Sites:\t" + allocationSites);
        System.out.println(options.statsFactory());
    }

    @Override
    protected void backwardSolve(BackwardQuery query) {
    }

    @Override
    public BackwardBoomerangResults<W> backwardSolveUnderScope(BackwardQuery backwardQuery, ForwardQuery forwardQuery,
            Node<Statement, Val> node) {
        return null;
    }
}
