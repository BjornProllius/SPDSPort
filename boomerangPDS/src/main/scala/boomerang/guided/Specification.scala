package boomerang.guided

import com.google.common.base.Objects
import com.google.common.collect.Sets

import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._
import scala.util.matching.Regex

class Specification{
  private val ON_SELECTOR = "ON"
  private val GO_SELECTOR = "GO"
  private val BACKWARD = "{B}"
  private val FORWARD = "{F}"

  object QueryDirection extends Enumeration {
    type QueryDirection = Value
    val FORWARD, BACKWARD = Value
  }

  private val methodAndQueries: Set[SootMethodWithSelector]

  private def this(spec: Iterable[String]) {
    this()
    methodAndQueries = spec.map(x => parse(x)).toSet
  }

  private def parse(input: String): SootMethodWithSelector = {
    val arguments = "\\((.*?)\\)".r
    val argumentMatcher = arguments.findAllIn(input)
    var on = Set[QuerySelector]()
    var go = Set[QuerySelector]()

    // Handle arguments
    if (argumentMatcher.hasNext) {
      val group = argumentMatcher.group(1)
      val args = group.split(",")
      for (i <- args.indices) {
        createQuerySelector(args(i), Parameter.of(i), on, go)
      }
    }

    // Handle base variable
    val base = "<(.*?):".r
    val baseMatcher = base.findAllIn(input)
    if (baseMatcher.hasNext) {
      val group = baseMatcher.group(1)
      createQuerySelector(group, Parameter.base(), on, go)
    }

    // Handle return
    val s = input.split(" ")
    createQuerySelector(s(1), Parameter.returnParam(), on, go)

    var sootMethod =
      input
        .replace(FORWARD, "")
        .replace(BACKWARD, "")
        .replace(ON_SELECTOR, "")
        .replace(GO_SELECTOR, "")

    // Assert parsing successful
    val backwardQueryCount =
      on.count(x => x.direction == QueryDirection.BACKWARD) +
        go.count(x => x.direction == QueryDirection.BACKWARD)
    val forwardQueryCount =
      on.count(x => x.direction == QueryDirection.FORWARD) +
        go.count(x => x.direction == QueryDirection.FORWARD)
    if (input.length
      != sootMethod.length
        + (on.size * ON_SELECTOR.length
        + go.size * GO_SELECTOR.length
        + backwardQueryCount * BACKWARD.length
        + forwardQueryCount * FORWARD.length)) {
      throw new RuntimeException("Parsing Specification failed. Please check your specification")
    }

    new SootMethodWithSelector(sootMethod, on, go)
  }

  private def createQuerySelector(arg: String, p: Parameter, on: mutable.Set[QuerySelector], go: mutable.Set[QuerySelector]): Unit = {
    if (arg.startsWith(ON_SELECTOR)) {
      on.add(
        QuerySelector(
          if (arg.contains(FORWARD)) QueryDirection.FORWARD else QueryDirection.BACKWARD, p))
    }
    if (arg.startsWith(GO_SELECTOR)) {
      go.add(
        QuerySelector(
          if (arg.contains(FORWARD)) QueryDirection.FORWARD else QueryDirection.BACKWARD, p))
    }
  }

  case class SootMethodWithSelector(sootMethod: String, on: Collection[QuerySelector], go: Collection[QuerySelector])

  case class Parameter(value: Int) {
    require(value >= -2, "Parameter index must be >= -2")

    override def equals(obj: Any): Boolean = obj match {
      case Parameter(v) => value == v
      case _ => false
    }

    override def hashCode(): Int = value.hashCode()
  }

  object Parameter {
    def returnParam(): Parameter = Parameter(-2)
    def base(): Parameter = Parameter(-1)
    def of(integer: Int): Parameter = {
      if (integer < 0) {
        throw new RuntimeException("Parameter index must be > 0")
      }
      Parameter(integer)
    }
  }
case class QuerySelector(direction: QueryDirection, argumentSelection: Parameter)

object Specification {
  def loadFrom(filePath: String): Specification = {
    new Specification(Files.lines(Paths.get(filePath)).collect(Collectors.toSet()).asScala.toSet)
  }

  def create(spec: String*): Specification = {
    new Specification(Sets.newHashSet(spec: _*).asScala.toSet)
  }
}

class Specification {
  def getMethodAndQueries: Set[SootMethodWithSelector] = {
    methodAndQueries
  }
}
}