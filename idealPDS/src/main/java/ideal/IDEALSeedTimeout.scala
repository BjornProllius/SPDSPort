/**
  * ******************************************************************************
  * Copyright (c) 2018
  * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
  * available under the terms of the Eclipse Public License 2.0 which is available at
  * http://www.eclipse.org/legal/epl-2.0.
  *
  * SPDX-License-Identifier: EPL-2.0
  *
  * Contributors: Johannes Spaeth - initial API and implementation
  * ******************************************************************************
  */
  package ideal

  import boomerang.{WeightedBoomerang, ForwardBoomerangResults}
  import wpds.impl.Weight
  
  /** Created by johannesspath on 01.12.17. */
  class IDEALSeedTimeout[W <: Weight](
      val solver: IDEALSeedSolver[W],
      val timedoutSolver: WeightedBoomerang[W],
      val lastResults: ForwardBoomerangResults[W]
  ) extends RuntimeException {
  
    override def toString: String = "IDEAL Seed TimeoutException \n"
  }
  