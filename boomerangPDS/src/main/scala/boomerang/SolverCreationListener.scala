package boomerang

import boomerang.solver.AbstractBoomerangSolver
import wpds.impl.Weight

trait SolverCreationListener[W <: Weight] {
  def onCreatedSolver(query: Query, solver: AbstractBoomerangSolver[W]): Unit
}