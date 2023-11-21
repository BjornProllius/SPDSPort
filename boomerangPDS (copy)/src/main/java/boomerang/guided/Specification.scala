package boomerang.guided

import com.google.common.base.Objects
import com.google.common.collect.Sets
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import java.util
import java.util.regex.Matcher
import java.util.regex.Pattern
import java.util.stream.Collectors

object Specification {
  private val ON_SELECTOR = "ON"
  private val GO_SELECTOR = "GO"
  private val BACKWARD = "{B}"
  private val FORWARD = "{F}"

  object QueryDirection extends Enumeration {
    type QueryDirection = Value
    val FORWARD, BACKWARD = Value
  }

  class SootMethodWithSelector private[guided](private var sootMethod: Nothing, private var on: Nothing, private var go: Nothing) {
    def getOn: Nothing = on

    def getGo: Nothing = go

    def getSootMethod: Nothing = sootMethod
  }

  object Parameter {
    def returnParam = new Specification.Parameter(-2)

    def base = new Specification.Parameter(-1)

    def of(integer: Int): Specification.Parameter = {
      if (integer < 0) throw new Nothing("Parameter index must be > 0")
      new Specification.Parameter(integer)
    }
  }

  class Parameter private(private val value: Int) {
    def getValue: Int = value

    @Override def equals(o: Nothing): Boolean = {
      if (this eq o) return true
      if (o == null || (getClass ne o.getClass)) return false
      val parameter = o.asInstanceOf[Specification.Parameter]
      value == parameter.value
    }

    @Override def hashCode: Int = Objects.hashCode(value)
  }

  @throws[IOException]
  def loadFrom(filePath: Nothing) = new Specification(Files.lines(Paths.get(filePath)).collect(Collectors.toSet))

  def create(spec: Nothing*) = new Specification(Sets.newHashSet(spec))
}

class Specification private(spec: Nothing) {
  methodAndQueries = spec.stream.map((x) => parse(x)).collect(Collectors.toSet)
  final private var methodAndQueries: Nothing = null

  private def parse(input: Nothing) = {
    val arguments = Pattern.compile("\\((.*?)\\)")
    val argumentMatcher = arguments.matcher(input)
    val on = Sets.newHashSet
    val go = Sets.newHashSet
    // Handle arguments
    if (argumentMatcher.find) {
      val group = argumentMatcher.group(1)
      val args = group.split(",")
      for (i <- 0 until args.length) {
        createQuerySelector(args(i), Specification.Parameter.of(i), on, go)
      }
    }
    // Handle base variable
    val base = Pattern.compile("<(.*?):")
    val baseMatcher = base.matcher(input)
    if (baseMatcher.find) {
      val group = baseMatcher.group(1)
      createQuerySelector(group, Specification.Parameter.base, on, go)
    }
    // Handle return
    val s = input.split(" ")
    createQuerySelector(s(1), Specification.Parameter.returnParam, on, go)
    val sootMethod = input.replace(Specification.FORWARD, "").replace(Specification.BACKWARD, "").replace(Specification.ON_SELECTOR, "").replace(Specification.GO_SELECTOR, "")
    // Assert parsing successful
    val backwardQueryCount = on.stream.filter((x) => x.direction eq Specification.QueryDirection.BACKWARD).count + go.stream.filter((x) => x.direction eq Specification.QueryDirection.BACKWARD).count
    val forwardQueryCount = on.stream.filter((x) => x.direction eq Specification.QueryDirection.FORWARD).count + go.stream.filter((x) => x.direction eq Specification.QueryDirection.FORWARD).count
    if (input.length ne sootMethod.length + (on.size * Specification.ON_SELECTOR.length + go.size * Specification.GO_SELECTOR.length + backwardQueryCount * Specification.BACKWARD.length + forwardQueryCount * Specification.FORWARD.length)) throw new Nothing("Parsing Specification failed. Please check your specification")
    new Specification.SootMethodWithSelector(sootMethod, on, go)
  }

  private def createQuerySelector(arg: Nothing, p: Specification.Parameter, on: Nothing, go: Nothing): Unit = {
    if (arg.startsWith(Specification.ON_SELECTOR)) on.add(new Specification#QuerySelector(if (arg.contains(Specification.FORWARD)) Specification.QueryDirection.FORWARD
    else Specification.QueryDirection.BACKWARD, p))
    if (arg.startsWith(Specification.GO_SELECTOR)) go.add(new Specification#QuerySelector(if (arg.contains(Specification.FORWARD)) Specification.QueryDirection.FORWARD
    else Specification.QueryDirection.BACKWARD, p))
  }

  class QuerySelector private[guided](private[guided] val direction: Specification.QueryDirection, private[guided] val argumentSelection: Specification.Parameter) {
  }

  def getMethodAndQueries: Nothing = methodAndQueries
}