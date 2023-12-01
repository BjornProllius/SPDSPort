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
package inference;

import boomerang.jimple.Statement;
import boomerang.jimple.Val;
import soot.Local;
import soot.SootMethod;
import sync.pds.solver.WeightFunctions;
import sync.pds.solver.nodes.Node;

public class InferenceWeightFunctions implements WeightFunctions<Statement, Val, Statement, InferenceWeight> {

    @Override
    public InferenceWeight push(Node<Statement, Val> curr, Node<Statement, Val> succ, Statement field) {
        SootMethod callee = succ.stmt().getMethod();
        if (callee.hasActiveBody()) {
            if (!callee.isStatic()) {
                Local thisLocal = callee.getActiveBody().getThisLocal();
                if (succ.fact().value().equals(thisLocal)) {
                    return new InferenceWeight(callee);
                }
            }
        }
        return getOne();
    }

    @Override
    public InferenceWeight normal(Node<Statement, Val> curr, Node<Statement, Val> succ) {
        return getOne();
    }

    @Override
    public InferenceWeight pop(Node<Statement, Val> curr, Statement location) {
        return getOne();
    }

    @Override
    public InferenceWeight getOne() {
        return InferenceWeight.one();
    }

    @Override
    public InferenceWeight getZero() {
        return InferenceWeight.zero();
    }

}
