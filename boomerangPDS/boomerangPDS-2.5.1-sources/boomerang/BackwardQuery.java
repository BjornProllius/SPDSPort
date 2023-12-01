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

import boomerang.jimple.Statement;
import boomerang.jimple.Val;

public class BackwardQuery extends Query {
    public BackwardQuery(Statement stmt, Val variable) {
        super(stmt, variable);
    }

    @Override
    public String toString() {
        return "BackwardQuery: " + super.toString();
    }
}