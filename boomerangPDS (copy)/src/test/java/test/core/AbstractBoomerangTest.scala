package test.core

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.BoomerangOptions
import boomerang.DefaultBoomerangOptions
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.WeightedBoomerang
import boomerang.WholeProgramBoomerang
import boomerang.results.BackwardBoomerangResults
import boomerang.scene.AllocVal
import boomerang.scene.CallGraph
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.DataFlowScope
import boomerang.scene.Field
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.IntAndStringBoomerangOptions
import boomerang.scene.jimple.SootCallGraph
import boomerang.solver.ForwardBoomerangSolver
import boomerang.util.AccessPath
import boomerang.util.DefaultValueMap
import com.google.common.base.Joiner
import com.google.common.base.Stopwatch
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.math.BigInteger
import java.time.Duration
import java.util
import java.util.stream.Collectors
import org.junit.Before
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import soot.Scene
import soot.SceneTransformer
import sync.pds.solver.OneWeightFunctions
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import test.core.selfrunning.AbstractTestingFramework
import wpds.impl.Transition
import wpds.impl.Weight.NoWeight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener

object AbstractBoomerangTest {
  /**
   * Fails the test cases, when any instance of the interface {@link
 * test.core.selfrunning.NoAllocatedObject} is detected.
   */
  private val FAIL_ON_IMPRECISE = true
  /**
   * Fails the test cases, when Boomerang's result set contains any object that does not inherit
   * from {@link test.core.selfrunning.AllocatedObject}.
   */
  private val TRACK_IMPLICIT_IMPRECISE = false
  private val LOGGER = LoggerFactory.getLogger(classOf[AbstractBoomerangTest])
  private var globalQueryTime = Duration.ofMillis(0)

  final class AnalysisMode {}

  /**
   * The methods parameter describes the variable that a query is issued for. Note: We misuse
   * the @Deprecated annotation to highlight the method in the Code.
   */
  def queryFor(variable: Nothing): Unit = {
  }

  def accessPathQueryFor(variable: Nothing, aliases: Nothing): Unit = {
  }

  def intQueryFor(variable: Int, value: Nothing): Unit = {
  }

  def intQueryFor(variable: Nothing, value: Nothing): Unit = {
  }
}

class AbstractBoomerangTest extends Nothing {
  private var queryDetector: Nothing = null
  private var expectedAllocationSites: Nothing = null
  private var explicitlyUnexpectedAllocationSites: Nothing = null
  protected var queryForCallSites: Nothing = null
  protected var unsoundErrors: Nothing = Sets.newHashSet
  protected var imprecisionErrors: Nothing = Sets.newHashSet
  protected var analysisTimeout: Int = 3000 * 1000
  private var callGraph: Nothing = null
  private var dataFlowScope: Nothing = null

  protected def getAnalyses: Array[AbstractBoomerangTest.AnalysisMode] = Array[AbstractBoomerangTest.AnalysisMode](
    // AnalysisMode.WholeProgram,
    AbstractBoomerangTest.AnalysisMode.DemandDrivenBackward)

  def getIterations = 1

  @Before def beforeTestCaseExecution(): Unit = {
    super.beforeTestCaseExecution
  }

  protected def createAnalysisTransformer: Nothing = new Nothing() {
    protected def internalTransform(phaseName: Nothing, @SuppressWarnings("rawtypes") options: Nothing): Unit = {
      BoomerangPretransformer.v.reset
      BoomerangPretransformer.v.apply
      callGraph = new Nothing
      dataFlowScope = getDataFlowScope
      analyzeWithCallGraph()
    }
  }

  private def analyzeWithCallGraph(): Unit = {
    queryDetector = new Nothing(callGraph)
    queryForCallSites = queryDetector.computeSeeds
    if (queryDetector.integerQueries) {
      val an = new Nothing(callGraph, new Nothing)
      expectedAllocationSites = an.computeSeeds
    }
    else {
      var an = new Nothing(callGraph, new Nothing("test.core.selfrunning.AllocatedObject"))
      expectedAllocationSites = an.computeSeeds
      an = new Nothing(callGraph, new Nothing("test.core.selfrunning.NoAllocatedObject"))
      explicitlyUnexpectedAllocationSites = an.computeSeeds.stream.map((x) => x.asNode).collect(Collectors.toList)
    }
    for (i <- 0 until getIterations) {
      for (analysis <- getAnalyses) {
        analysis match {
          case WholeProgram =>
            if (!queryDetector.integerQueries) runWholeProgram()
          case DemandDrivenBackward =>
            runDemandDrivenBackward()
        }
      }
      if (queryDetector.resultsMustNotBeEmpty) return
      if (!unsoundErrors.isEmpty) throw new Nothing(Joiner.on("\n").join(unsoundErrors))
      if (!imprecisionErrors.isEmpty && AbstractBoomerangTest.FAIL_ON_IMPRECISE) throw new Nothing(Joiner.on("\n").join(imprecisionErrors))
    }
  }

  private def runWholeProgram(): Unit = {
    val results = Sets.newHashSet
    val solver = new Nothing((callGraph, dataFlowScope, new Nothing() {
      @Override def analysisTimeoutMS: Int = analysisTimeout

      @Override def onTheFlyCallGraph = false
    })) {
      @Override protected def getForwardFieldWeights = new Nothing(NoWeight.NO_WEIGHT_ONE)

      @Override protected def getBackwardFieldWeights = new Nothing(NoWeight.NO_WEIGHT_ONE)

      @Override protected def getBackwardCallWeights = new Nothing(NoWeight.NO_WEIGHT_ONE)

      @Override protected def getForwardCallWeights(sourceQuery: Nothing) = new Nothing(NoWeight.NO_WEIGHT_ONE)
    }
    solver.wholeProgramAnalysis
    val solvers = solver.getSolvers
    import scala.collection.JavaConversions._
    for (q <- solvers.keySet) {
      import scala.collection.JavaConversions._
      for (queryForCallSite <- queryForCallSites) {
        solvers.get(q).getFieldAutomaton.registerListener(new Nothing(new Nothing(queryForCallSite.asNode)) {
          @Override def onOutTransitionAdded(t: Nothing, w: Nothing, weightedPAutomaton: Nothing): Unit = {
            if (t.getLabel.equals(Field.empty) && t.getTarget.fact.equals(q.asNode)) results.add(q.asNode)
          }

          @Override def onInTransitionAdded(t: Nothing, w: Nothing, weightedPAutomaton: Nothing): Unit = {
          }
        })
      }
      import scala.collection.JavaConversions._
      for (s <- solvers.get(q).getReachedStates) {
        if (s.stmt.getMethod.toString.contains("unreachable") && !q.toString.contains("dummyClass.main")) throw new Nothing("Propagation within unreachable method found: " + q)
      }
    }
    compareQuery(expectedAllocationSites, results, AbstractBoomerangTest.AnalysisMode.WholeProgram)
    System.out.println
  }

  private def runDemandDrivenBackward(): Unit = {
    // Run backward analysis
    val backwardResults = runQuery(queryForCallSites)
    if (queryDetector.integerQueries) compareIntegerResults(backwardResults, AbstractBoomerangTest.AnalysisMode.DemandDrivenBackward)
    else compareQuery(expectedAllocationSites, backwardResults, AbstractBoomerangTest.AnalysisMode.DemandDrivenBackward)
  }

  private def compareIntegerResults(backwardResults: Nothing, analysis: AbstractBoomerangTest.AnalysisMode): Unit = {
    if (queryForCallSites.size > 1) throw new Nothing("Not implemented")
    import scala.collection.JavaConversions._
    for (q <- queryForCallSites) {
      val stmt = q.cfgEdge
      val ie = stmt.getStart.getInvokeExpr
      val arg = ie.getArg(1)
      val expectedResults = parse(arg)
      AbstractBoomerangTest.LOGGER.info("Expected results: {}", expectedResults)
      var imprecise = false
      import scala.collection.JavaConversions._
      for (v <- backwardResults) {
        if (v.fact.isInstanceOf[Nothing]) {
          val allocVal = v.fact.asInstanceOf[Nothing]
          val remove = expectedResults.remove(allocVal.toString)
          if (!remove) imprecise = true
        }
        else imprecise = true
      }
      if (!expectedResults.isEmpty) unsoundErrors.add(new Nothing(analysis + " Unsound results!"))
      if (imprecise) imprecisionErrors.add(new Nothing(analysis + " Imprecise results!"))
    }
  }

  private def parse(arg: Nothing) = {
    val split = arg.getStringValue.split(",")
    Lists.newArrayList(split)
  }

  private def runQuery(queries: Nothing) = {
    val results = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (query <- queries) {
      val options = createBoomerangOptions
      val solver = new Nothing((callGraph, getDataFlowScope, options)) {}
      if (query.isInstanceOf[Nothing]) {
        val watch = Stopwatch.createStarted
        val res = solver.solve(query.asInstanceOf[Nothing])
        AbstractBoomerangTest.globalQueryTime = AbstractBoomerangTest.globalQueryTime.plus(watch.elapsed)
        AbstractBoomerangTest.LOGGER.info("Solving query took: {}", watch)
        AbstractBoomerangTest.LOGGER.info("Expected results: {}", AbstractBoomerangTest.globalQueryTime)
        import scala.collection.JavaConversions._
        for (q <- res.getAllocationSites.keySet) {
          results.add(q.asNode)
          import scala.collection.JavaConversions._
          for (s <- solver.getSolvers.get(q).getReachedStates) {
            if (s.stmt.getMethod.toString.contains("unreachable")) throw new Nothing("Propagation within unreachable method found.")
          }
        }
        if (queryDetector.accessPathQuery) checkContainsAllExpectedAccessPath(res.getAllAliases)
      }
    }
    results
  }

  protected def getDataFlowScope: Nothing = SootDataFlowScope.make(Scene.v)

  protected def createBoomerangOptions: Nothing = if (queryDetector.integerQueries) new Nothing
  else new Nothing() {
    @Override def analysisTimeoutMS: Int = return analysisTimeout
  }

  private def compareQuery(expectedResults: Nothing, results: Nothing, analysis: AbstractBoomerangTest.AnalysisMode): Unit = {
    AbstractBoomerangTest.LOGGER.info("Boomerang Results: {}", results)
    AbstractBoomerangTest.LOGGER.info("Expected Results: {}", expectedResults)
    val falseNegativeAllocationSites = new Nothing
    import scala.collection.JavaConversions._
    for (res <- expectedResults) {
      if (!results.contains(res.asNode)) falseNegativeAllocationSites.add(res.asNode)
    }
    val falsePositiveAllocationSites = new Nothing(results)
    import scala.collection.JavaConversions._
    for (res <- expectedResults) {
      falsePositiveAllocationSites.remove(res.asNode)
    }
    val answer = (if (falseNegativeAllocationSites.isEmpty) ""
    else "\nFN:" + falseNegativeAllocationSites) + (if (falsePositiveAllocationSites.isEmpty) ""
    else "\nFP:" + falsePositiveAllocationSites + "\n")
    if (!falseNegativeAllocationSites.isEmpty) unsoundErrors.add(new Nothing(analysis + " Unsound results for:" + answer))
    if (AbstractBoomerangTest.TRACK_IMPLICIT_IMPRECISE && !falsePositiveAllocationSites.isEmpty) imprecisionErrors.add(new Nothing(analysis + " Imprecise results for:" + answer))
    if (queryDetector.resultsMustNotBeEmpty && results.isEmpty) throw new Nothing("Expected some results, but Boomerang returned no allocation sites.")
    import scala.collection.JavaConversions._
    for (r <- results) {
      if (explicitlyUnexpectedAllocationSites.contains(r)) imprecisionErrors.add(new Nothing(analysis + " Imprecise results for:" + answer))
    }
  }

  private def checkContainsAllExpectedAccessPath(allAliases: Nothing): Unit = {
    val expected = Sets.newHashSet(queryDetector.expectedAccessPaths)
    expected.removeAll(allAliases)
    if (!expected.isEmpty) throw new Nothing("Did not find all access path! " + expected)
  }

  protected def errorOnVisitMethod: Nothing = Lists.newLinkedList

  protected def includeJDK = true

  protected def queryForAndNotEmpty(variable: Nothing): Unit = {
  }

  /**
   * A call to this method flags the object as at the call statement as not reachable by the
   * analysis.
   *
   * @param variable
   */
  protected def unreachable(variable: Nothing): Unit = {
  }

  /**
   * This method can be used in test cases to create branching. It is not optimized away.
   *
   * @return
   */
  protected def staticallyUnknown = true

  protected def setupSolver(solver: Nothing): Unit = {
  }
}