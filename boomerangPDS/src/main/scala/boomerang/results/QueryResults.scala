package boomerang.results

import boomerang.Query
import boomerang.scene.Method

class QueryResults(
    var query: Query,
    val visitedMethods: Set[Method],
    val affectedLocations: Set[AffectedLocation],
    val timedout: Boolean
) {

  def getQuery(): Query = query

  def getVisitedMethods(): Set[Method] = visitedMethods

  def getAffectedLocations(): Set[AffectedLocation] = affectedLocations

  def isTimedout(): Boolean = timedout
}