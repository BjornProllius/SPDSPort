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
package boomerang.solver;

import boomerang.jimple.Field;
import boomerang.jimple.Statement;
import boomerang.jimple.Val;
import soot.SootMethod;
import sync.pds.solver.nodes.INode;
import sync.pds.solver.nodes.Node;
import wpds.impl.Transition;
import wpds.impl.Weight;
import wpds.impl.WeightedPAutomaton;
import wpds.interfaces.WPAUpdateListener;

public abstract class MethodBasedFieldTransitionListener<W extends Weight>
        implements WPAUpdateListener<Field, INode<Node<Statement, Val>>, W> {
    private final SootMethod method;

    public MethodBasedFieldTransitionListener(SootMethod method) {
        this.method = method;
    }

    public SootMethod getMethod() {
        return method;
    }

    @Override
    public void onWeightAdded(Transition<Field, INode<Node<Statement, Val>>> t, W w,
            WeightedPAutomaton<Field, INode<Node<Statement, Val>>, W> aut) {
        onAddedTransition(t);
    }

    public abstract void onAddedTransition(Transition<Field, INode<Node<Statement, Val>>> t);
}
