package boomerang

import boomerang.solver.AbstractBoomerangSolver
import wpds.impl.Weight

trait SolverCreationListener[W <: Weight] {
  def onCreatedSolver(query: Nothing, solver: Nothing): Unit
}