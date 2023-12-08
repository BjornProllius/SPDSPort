package boomerang.solver

import boomerang.{BoomerangOptions, StaticFieldStrategy}
import boomerang.arrays.{ArrayHandlingStrategy, ArrayIndexInsensitiveStrategy, ArrayIndexSensitiveStrategy, IgnoreArrayStrategy}
import boomerang.staticfields.{FlowSensitiveStaticFieldStrategy, IgnoreStaticFieldStrategy, SingletonStaticFieldStrategy}
import boomerang.scene.{Field, Statement}
import com.google.common.collect.Multimap
import wpds.impl.Weight

class Strategies[W <: Weight](opts: BoomerangOptions, solver: AbstractBoomerangSolver, fieldLoadStatements: Multimap[Field, Statement], fieldStoreStatements: Multimap[Field, Statement]) {
  private val staticFieldStrategy: StaticFieldStrategy[W] = opts.getStaticFieldStrategy match {
    case BoomerangOptions.StaticFieldStrategy.IGNORE =>
      new IgnoreStaticFieldStrategy()
    case BoomerangOptions.StaticFieldStrategy.SINGLETON =>
      new SingletonStaticFieldStrategy[W](solver, fieldLoadStatements, fieldStoreStatements)
    case _ =>
      new FlowSensitiveStaticFieldStrategy()
  }

  private val arrayHandlingStrategy: ArrayHandlingStrategy[W] = opts.getArrayStrategy match {
    case BoomerangOptions.ArrayStrategy.DISABLED =>
      new IgnoreArrayStrategy()
    case BoomerangOptions.ArrayStrategy.INDEX_INSENSITIVE =>
      new ArrayIndexInsensitiveStrategy()
    case _ =>
      new ArrayIndexSensitiveStrategy()
  }

  def getStaticFieldStrategy: StaticFieldStrategy[W] = staticFieldStrategy

  def getArrayHandlingStrategy: ArrayHandlingStrategy[W] = arrayHandlingStrategy
}