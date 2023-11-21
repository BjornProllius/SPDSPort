package boomerang.weights

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Val
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import sync.pds.solver.nodes.Node
import wpds.impl.Weight

object PathTrackingWeight {
  private var one: PathTrackingWeight = null

  def one: PathTrackingWeight = {
    if (one == null) one = new PathTrackingWeight("ONE")
    one
  }
}

class PathTrackingWeight extends Nothing {
  /**
   * This set keeps track of all statements on a shortest path that use an alias from source to
   * sink.
   */
    private var shortestPathWitness = new Nothing
  /**
   * This set keeps track of all statement along all paths that use an alias from source to sink.
   */
  private var allPathWitness = Sets.newHashSet
  private var rep: Nothing = null

  def this(rep: Nothing) {
    this()
    this.rep = rep
  }

  def this(allStatement: Nothing, allPathWitness: Nothing) {
    this()
    this.shortestPathWitness = allStatement
    this.allPathWitness = allPathWitness
  }

  def this(relevantStatement: Nothing) {
    this()
    this.shortestPathWitness.add(relevantStatement)
    val firstDataFlowPath = new Nothing
    firstDataFlowPath.add(relevantStatement)
    this.allPathWitness.add(firstDataFlowPath)
  }

  @Override def extendWith(o: Nothing): Nothing = {
    if (!o.isInstanceOf[PathTrackingWeight]) throw new Nothing("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[PathTrackingWeight]
    val newAllStatements = new Nothing
    newAllStatements.addAll(shortestPathWitness)
    newAllStatements.addAll(other.shortestPathWitness)
    val newAllPathStatements = new Nothing
    import scala.collection.JavaConversions._
    for (pathPrefix <- allPathWitness) {
      import scala.collection.JavaConversions._
      for (pathSuffix <- other.allPathWitness) {
        val combinedPath = Sets.newLinkedHashSet
        combinedPath.addAll(pathPrefix)
        combinedPath.addAll(pathSuffix)
        newAllPathStatements.add(combinedPath)
      }
    }
    if (allPathWitness.isEmpty) {
      import scala.collection.JavaConversions._
      for (pathSuffix <- other.allPathWitness) {
        val combinedPath = Sets.newLinkedHashSet
        combinedPath.addAll(pathSuffix)
        newAllPathStatements.add(combinedPath)
      }
    }
    if (other.allPathWitness.isEmpty) {
      import scala.collection.JavaConversions._
      for (pathSuffix <- allPathWitness) {
        val combinedPath = Sets.newLinkedHashSet
        combinedPath.addAll(pathSuffix)
        newAllPathStatements.add(combinedPath)
      }
    }
    new PathTrackingWeight(newAllStatements, newAllPathStatements)
  }

  @Override def combineWith(o: Nothing): Nothing = {
    if (!o.isInstanceOf[PathTrackingWeight]) throw new Nothing("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[PathTrackingWeight]
    val newAllPathStatements = new Nothing
    import scala.collection.JavaConversions._
    for (pathPrefix <- allPathWitness) {
      val combinedPath = Sets.newLinkedHashSet
      combinedPath.addAll(pathPrefix)
      newAllPathStatements.add(combinedPath)
    }
    import scala.collection.JavaConversions._
    for (pathPrefix <- other.allPathWitness) {
      val combinedPath = Sets.newLinkedHashSet
      combinedPath.addAll(pathPrefix)
      newAllPathStatements.add(combinedPath)
    }
    if (shortestPathWitness.size > other.shortestPathWitness.size) return new PathTrackingWeight(new Nothing(other.shortestPathWitness), newAllPathStatements)
    new PathTrackingWeight(new Nothing(this.shortestPathWitness), newAllPathStatements)
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (shortestPathWitness == null) 0
    else shortestPathWitness.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[PathTrackingWeight]
    if (shortestPathWitness == null) if (other.shortestPathWitness != null) return false
    else if (!shortestPathWitness.equals(other.shortestPathWitness)) return false
    if (allPathWitness == null) if (other.allPathWitness != null) return false
    else if (!allPathWitness.equals(other.allPathWitness)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    true
  }

  @Override def toString: Nothing = "\nAll statements: " + shortestPathWitness

  def getShortestPathWitness: Nothing = Lists.newArrayList(shortestPathWitness)

  def getAllPathWitness: Nothing = Sets.newHashSet(allPathWitness)
}