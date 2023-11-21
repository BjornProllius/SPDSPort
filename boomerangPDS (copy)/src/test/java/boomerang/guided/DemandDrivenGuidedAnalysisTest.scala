package boomerang.guided

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.QueryGraph
import boomerang.guided.targets.ArrayContainerTarget
import boomerang.guided.targets.BasicTarget
import boomerang.guided.targets.BranchingAfterNewStringTest
import boomerang.guided.targets.BranchingTest
import boomerang.guided.targets.ContextSensitiveAndLeftUnbalanced2StacksTarget
import boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedFieldTarget
import boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedTarget
import boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedTarget2
import boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedThisFieldTarget
import boomerang.guided.targets.ContextSensitiveTarget
import boomerang.guided.targets.IntegerCastTarget
import boomerang.guided.targets.LeftUnbalancedTarget
import boomerang.guided.targets.NestedContextAndBranchingTarget
import boomerang.guided.targets.NestedContextTarget
import boomerang.guided.targets.PingPongInterproceduralTarget
import boomerang.guided.targets.PingPongTarget
import boomerang.guided.targets.ValueOfTarget
import boomerang.guided.targets.WrappedInNewStringInnerTarget
import boomerang.guided.targets.WrappedInNewStringTarget
import boomerang.guided.targets.WrappedInStringTwiceTest
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.IntAndStringBoomerangOptions
import boomerang.scene.jimple.JimpleMethod
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.nio.file.Path
import java.nio.file.Paths
import java.util
import java.util.Optional
import java.util.stream.Collectors
import java.util.stream.Stream
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test
import soot.G
import soot.PackManager
import soot.Scene
import soot.SootClass
import soot.SootMethod
import soot.options.Options
import wpds.impl.Weight.NoWeight

object DemandDrivenGuidedAnalysisTest {
  var CG = "cha"

  def selectFirstArgOfQueryTarget(m: Nothing): Nothing = {
    val method = JimpleMethod.of(m)
    method.getStatements.stream.filter((x) => x.containsInvokeExpr).forEach((x) => x.toString)
    val newFileStatement = method.getStatements.stream.filter((x) => x.containsInvokeExpr).filter((x) => x.getInvokeExpr.getMethod.getName.equals("queryFor") && x.getInvokeExpr.getMethod.getDeclaringClass.getFullyQualifiedName.equals("boomerang.guided.targets.Query")).findFirst.get
    val arg = newFileStatement.getInvokeExpr.getArg(0)
    val predecessor = method.getControlFlowGraph.getPredsOf(newFileStatement).stream.findFirst.get
    val cfgEdge = new Nothing(predecessor, newFileStatement)
    BackwardQuery.make(cfgEdge, arg)
  }

  def selectFirstFileInitArgument(m: Nothing): Nothing = {
    val method = JimpleMethod.of(m)
    method.getStatements.stream.filter((x) => x.containsInvokeExpr).forEach((x) => x.toString)
    val newFileStatement = method.getStatements.stream.filter((x) => x.containsInvokeExpr).filter((x) => x.getInvokeExpr.getMethod.getName.equals("<init>") && x.getInvokeExpr.getMethod.getDeclaringClass.getFullyQualifiedName.equals("java.io.File")).findFirst.get
    val arg = newFileStatement.getInvokeExpr.getArg(0)
    val predecessor = method.getControlFlowGraph.getPredsOf(newFileStatement).stream.findFirst.get
    val cfgEdge = new Nothing(predecessor, newFileStatement)
    BackwardQuery.make(cfgEdge, arg)
  }

  def selectFirstBaseOfToString(m: Nothing): Nothing = {
    val method = JimpleMethod.of(m)
    method.getStatements.stream.filter((x) => x.containsInvokeExpr).forEach((x) => x.toString)
    val newFileStatement = method.getStatements.stream.filter((x) => x.containsInvokeExpr).filter((x) => x.getInvokeExpr.getMethod.getName.equals("toString")).findFirst.get
    val arg = newFileStatement.getInvokeExpr.getBase
    val predecessor = method.getControlFlowGraph.getPredsOf(newFileStatement).stream.findFirst.get
    val cfgEdge = new Nothing(predecessor, newFileStatement)
    BackwardQuery.make(cfgEdge, arg)
  }
}

class DemandDrivenGuidedAnalysisTest {
  @Test def integerCastTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.IntegerCastTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstBaseOfToString(m)
    val spec = Specification.create("<java.lang.Integer: ON{B}java.lang.Integer valueOf(GO{B}int)>")
    runAnalysis(spec, query, 1)
  }

  @Test def basicTarget(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.BasicTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test
  @Ignore("We need additional logic to tell the analysis to continue at some unknown parent context") def leftUnbalancedTargetTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.LeftUnbalancedTarget: void bar(java.lang.String)>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def contextSensitiveTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ContextSensitiveTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def nestedContextTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.NestedContextTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def nestedContextAndBranchingTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.NestedContextAndBranchingTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar", "foo")
  }

  @Test def contextSensitiveAndLeftUnbalanced2StacksTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ContextSensitiveAndLeftUnbalanced2StacksTarget: void context()>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def contextSensitiveAndLeftUnbalancedTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedTarget: void context(java.lang.String)>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def contextSensitiveAndLeftUnbalancedWithFieldTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedFieldTarget: void context(java.lang.String)>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def contextSensitiveAndLeftUnbalancedWithThisFieldTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedThisFieldTarget$MyObject: void context()>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def contextSensitiveAndLeftUnbalancedWithFieldTest2(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ContextSensitiveAndLeftUnbalancedTarget2: void context()>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstBaseOfToString(m)
    runAnalysis(query, "bar")
  }

  @Test def wrappedInNewStringTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.WrappedInNewStringTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def wrappedInNewStringInnerTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.WrappedInNewStringInnerTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def wrappedInNewStringTwiceTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.WrappedInStringTwiceTest: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar")
  }

  @Test def branchingTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.BranchingTest: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar", "foo")
  }

  @Test def branchingAfterNewTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.BranchingAfterNewStringTest: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(query, "bar", "foo")
  }

  @Test def pingPongTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.PingPongTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(getPingPongSpecification, query, "hello", "world")
  }

  @Test def pingPongInterpoceduralTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.PingPongInterproceduralTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstFileInitArgument(m)
    runAnalysis(getPingPongSpecification, query, "hello", "world")
  }

  @Test def arrayContainerTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ArrayContainerTarget: void main(java.lang.String[])>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstBaseOfToString(m)
    runAnalysis(new Nothing, query, "hello", "world")
  }

  @Test def valueOfTarget(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.ValueOfTarget: void foo(int,int)>")
    val query = DemandDrivenGuidedAnalysisTest.selectFirstArgOfQueryTarget(m)
    runAnalysis(query, 1)
  }

  private def getPingPongSpecification = Specification.create("<ON{B}java.lang.StringBuilder: java.lang.StringBuilder append(GO{B}java.lang.String)>", "<ON{F}java.lang.StringBuilder: java.lang.StringBuilder append(GO{B}java.lang.String)>", "<ON{F}java.lang.StringBuilder: GO{F}java.lang.StringBuilder append(java.lang.String)>", "<GO{B}java.lang.StringBuilder: ON{B}java.lang.String toString()>")

  protected def runAnalysis(query: Nothing, expectedValues: Nothing*): Unit = {
    val specification = Specification.create("<java.lang.Integer: ON{B}java.lang.Integer valueOf(GO{B}int)>", "<ON{B}java.lang.Integer: java.lang.Integer <init>(GO{B}int)>", "<GO{F}java.lang.String: void <init>(ON{F}java.lang.String)>", "<ON{B}java.lang.String: void <init>(GO{B}java.lang.String)>", "<GO{B}java.lang.String: ON{B}byte[] getBytes()>")
    runAnalysis(specification, query, expectedValues)
  }

  private def isStringOrIntAllocation(stmt: Nothing) = stmt.isAssign && (stmt.getRightOp.isIntConstant || stmt.getRightOp.isStringConstant)

  protected def runAnalysis(specification: Nothing, query: Nothing, expectedValues: Nothing*): Unit = {
    runAnalysis(new Nothing(specification), query, expectedValues)
  }

  protected def runAnalysis(queryManager: Nothing, query: Nothing, expectedValues: Nothing*): Unit = {
    val demandDrivenGuidedAnalysis = new Nothing(queryManager, new Nothing() {
      @Override def getAllocationVal(m: Nothing, stmt: Nothing, fact: Nothing): Nothing = {
        if (stmt.isAssign && stmt.getLeftOp.equals(fact) && isStringOrIntAllocation(stmt)) return Optional.of(new Nothing(stmt.getLeftOp, stmt, stmt.getRightOp))
        super.getAllocationVal(m, stmt, fact)
      }

      @Override def analysisTimeoutMS = 5000

      @Override def allowMultipleQueries = true
    })
    val queryGraph = demandDrivenGuidedAnalysis.run(query)
    demandDrivenGuidedAnalysis.cleanUp
    // Filter out query graph's node to only return the queries of interest (ForwardQueries &
    // String/Int Allocation sites).
    val res = queryGraph.getNodes.stream.filter((x) => x.isInstanceOf[Nothing] && isStringOrIntAllocation(x.asNode.stmt.getStart))
    Assert.assertEquals(Sets.newHashSet(expectedValues), res.map((t) => t.`var`.asInstanceOf[Nothing].getAllocVal).filter((x) => x.isStringConstant || x.isIntConstant).map((x) => if (x.isIntConstant) x.getIntValue
    else x.getStringValue).collect(Collectors.toSet))
  }

  protected def setupSoot(cls: Nothing): Unit = {
    G.v.reset
    setupSoot()
    setApplicationClass(cls)
    PackManager.v.runPacks
    BoomerangPretransformer.v.reset
    BoomerangPretransformer.v.apply
  }

  private def setupSoot(): Unit = {
    Options.v.set_whole_program(true)
    Options.v.setPhaseOption("cg." + DemandDrivenGuidedAnalysisTest.CG, "on")
    Options.v.setPhaseOption("cg." + DemandDrivenGuidedAnalysisTest.CG, "verbose:true")
    Options.v.set_output_format(Options.output_format_none)
    Options.v.set_no_bodies_for_excluded(true)
    Options.v.set_allow_phantom_refs(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.set_keep_line_number(true)
    Options.v.set_prepend_classpath(true)
    Options.v.set_process_dir(getProcessDir)
  }

  private def setApplicationClass(cls: Nothing): Unit = {
    Scene.v.loadNecessaryClasses
    val eps = Lists.newArrayList
    import scala.collection.JavaConversions._
    for (sootClass <- Scene.v.getClasses) {
      if (sootClass.toString.equals(cls.getName) || sootClass.toString.contains(cls.getName + "$")) {
        sootClass.setApplicationClass
        eps.addAll(sootClass.getMethods)
      }
    }
    Scene.v.setEntryPoints(eps)
  }

  private def getProcessDir = {
    val path = Paths.get("target/test-classes")
    Lists.newArrayList(path.toAbsolutePath.toString)
  }
}