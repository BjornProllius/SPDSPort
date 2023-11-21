package boomerang.solver

import boomerang.BoomerangOptions
import boomerang.arrays.ArrayHandlingStrategy
import boomerang.arrays.ArrayIndexInsensitiveStrategy
import boomerang.arrays.ArrayIndexSensitiveStrategy
import boomerang.arrays.IgnoreArrayStrategy
import boomerang.scene.Field
import boomerang.scene.Statement
import boomerang.staticfields.FlowSensitiveStaticFieldStrategy
import boomerang.staticfields.IgnoreStaticFieldStrategy
import boomerang.staticfields.SingletonStaticFieldStrategy
import boomerang.staticfields.StaticFieldStrategy
import com.google.common.collect.Multimap
import wpds.impl.Weight

class Strategies[W <: Weight](opts: Nothing, solver: Nothing, fieldLoadStatements: Nothing, fieldStoreStatements: Nothing) {
  opts.getStaticFieldStrategy match {
    case IGNORE =>
      staticFieldStrategy = new Nothing
    case SINGLETON =>
      staticFieldStrategy = new Nothing(solver, fieldLoadStatements, fieldStoreStatements)
    case FLOW_SENSITIVE =>
    case _ =>
      staticFieldStrategy = new Nothing
  }
  opts.getArrayStrategy match {
    case DISABLED =>
      arrayHandlingStrategy = new Nothing
    case INDEX_INSENSITIVE =>
      arrayHandlingStrategy = new Nothing
    case INDEX_SENSITIVE =>
    case _ =>
      arrayHandlingStrategy = new Nothing
  }
  final private var staticFieldStrategy: Nothing = null
  final private var arrayHandlingStrategy: Nothing = null

  def getStaticFieldStrategy: Nothing = staticFieldStrategy

  def getArrayHandlingStrategy: Nothing = arrayHandlingStrategy
}