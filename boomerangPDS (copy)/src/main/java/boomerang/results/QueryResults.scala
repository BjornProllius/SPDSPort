package boomerang.results

import boomerang.Query
import boomerang.scene.Method
import java.util

class QueryResults(private var query: Nothing, private val affectedLocations: Nothing, private val visitedMethods: Nothing, private val timedout: Boolean) {
  def getQuery: Nothing = query

  def getVisitedMethods: Nothing = visitedMethods

  def getAffectedLocations: Nothing = affectedLocations

  def isTimedout: Boolean = timedout
}