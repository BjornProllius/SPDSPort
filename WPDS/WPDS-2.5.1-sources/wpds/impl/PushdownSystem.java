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
package wpds.impl;

import wpds.impl.Weight.NoWeight;
import wpds.interfaces.Location;
import wpds.interfaces.State;

public class PushdownSystem<N extends Location, D extends State> extends WeightedPushdownSystem<N, D, NoWeight> {

    @Override
    public boolean addRule(Rule<N, D, NoWeight> rule) {
        if (!(rule instanceof UNormalRule) && !(rule instanceof UPopRule) && !(rule instanceof UPushRule))
            throw new RuntimeException("Trying to add a weighted rule to an unweighted PDS!");
        return super.addRule(rule);
    }

}