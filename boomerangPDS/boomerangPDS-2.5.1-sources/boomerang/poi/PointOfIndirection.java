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
package boomerang.poi;

import java.util.Set;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import boomerang.ForwardQuery;
import boomerang.Query;

public abstract class PointOfIndirection<Statement, Val, Field> {

    private Set<ForwardQuery> actualBaseAllocations = Sets.newHashSet();
    private Set<Query> flowAllocations = Sets.newHashSet();

    public abstract void execute(ForwardQuery baseAllocation, Query flowAllocation);

    public void addBaseAllocation(ForwardQuery baseAllocation) {
        if (actualBaseAllocations.add(baseAllocation)) {
            for (Query flowAllocation : Lists.newArrayList(flowAllocations)) {
                execute(baseAllocation, flowAllocation);
            }
        }
    }

    public void addFlowAllocation(Query flowAllocation) {
        if (flowAllocations.add(flowAllocation)) {
            for (ForwardQuery baseAllocation : Lists.newArrayList(actualBaseAllocations)) {
                execute(baseAllocation, flowAllocation);
            }
        }
    }

    public abstract Statement getStmt();
}
