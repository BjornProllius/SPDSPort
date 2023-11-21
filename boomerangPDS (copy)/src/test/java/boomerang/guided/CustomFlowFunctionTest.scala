package boomerang.guided

import boomerang.BackwardQuery
import boomerang.Boomerang
import boomerang.DefaultBoomerangOptions
import boomerang.ForwardQuery
import boomerang.flowfunction.IBackwardFlowFunction
import boomerang.flowfunction.IForwardFlowFunction
import boomerang.guided.flowfunction.CustomBackwardFlowFunction
import boomerang.guided.flowfunction.CustomForwardFlowFunction
import boomerang.guided.targets.CustomFlowFunctionTarget
import boomerang.results.BackwardBoomerangResults
import boomerang.results.ForwardBoomerangResults
import boomerang.scene.AllocVal
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.SootDataFlowScope
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.jimple.BoomerangPretransformer
import boomerang.scene.jimple.IntAndStringBoomerangOptions
import boomerang.scene.jimple.JimpleMethod
import boomerang.scene.jimple.SootCallGraph
import boomerang.solver.BackwardBoomerangSolver
import com.google.common.collect.Lists
import java.nio.file.Path
import java.nio.file.Paths
import java.util
import org.junit.Assert
import org.junit.Test
import soot.G
import soot.PackManager
import soot.Scene
import soot.SootClass
import soot.SootMethod
import soot.options.Options
import wpds.impl.Weight.NoWeight

object CustomFlowFunctionTest {
  var CG = "cha"

  def selectQueryForStatement(m: Nothing): Nothing = {
    val method = JimpleMethod.of(m)
    method.getStatements.stream.filter((x) => x.containsInvokeExpr).forEach((x) => x.toString)
    val queryStatement = method.getStatements.stream.filter((x) => x.containsInvokeExpr).filter((x) => x.getInvokeExpr.getMethod.getName.equals("queryFor")).findFirst.get
    val arg = queryStatement.getInvokeExpr.getArg(0)
    val predecessor = method.getControlFlowGraph.getPredsOf(queryStatement).stream.findFirst.get
    val cfgEdge = new Nothing(predecessor, queryStatement)
    BackwardQuery.make(cfgEdge, arg)
  }

  def selectFirstIntAssignment(m: Nothing): Nothing = {
    val method = JimpleMethod.of(m)
    method.getStatements.stream.forEach((x) => System.out.println(x.toString))
    val intAssignStmt = method.getStatements.stream.filter((x) => x.isAssign && !x.getLeftOp.getType.isRefType).findFirst.get
    val arg = new Nothing(intAssignStmt.getLeftOp, intAssignStmt, intAssignStmt.getRightOp)
    val succs = method.getControlFlowGraph.getSuccsOf(intAssignStmt).stream.findFirst.get
    val cfgEdge = new Nothing(intAssignStmt, succs)
    new Nothing(cfgEdge, arg)
  }
}

class CustomFlowFunctionTest {
  @Test def killOnSystemExitBackwardTestInteger(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.CustomFlowFunctionIntTarget: void main(java.lang.String[])>")
    val query = CustomFlowFunctionTest.selectQueryForStatement(m)
    val sootCallGraph = new Nothing
    val solver = new Nothing(sootCallGraph, SootDataFlowScope.make(Scene.v), new CustomFlowFunctionTest#CustomIntAndStringBoomerangOptions)
    System.out.println("Solving query: " + query)
    val backwardQueryResults = solver.solve(query)
    import scala.collection.JavaConversions._
    for (bw <- solver.getBackwardSolvers.values) {
      Assert.assertEquals(true, bw.getCallAutomaton.getTransitions.size < 3)
    }
    System.out.println(backwardQueryResults.getAllocationSites)
    // For the query no allocation site is found, as between queryFor and the allocation site there
    // exists a System.exit call.
    Assert.assertEquals(true, backwardQueryResults.isEmpty)
  }

  @Test def killOnSystemExitBackwardTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.CustomFlowFunctionTarget: void main(java.lang.String[])>")
    val query = CustomFlowFunctionTest.selectQueryForStatement(m)
    val sootCallGraph = new Nothing
    val solver = new Nothing(sootCallGraph, SootDataFlowScope.make(Scene.v), new CustomFlowFunctionTest#CustomBoomerangOptions)
    System.out.println("Solving query: " + query)
    val backwardQueryResults = solver.solve(query)
    System.out.println(backwardQueryResults.getAllocationSites)
    // For the query no allocation site is found, as between queryFor and the allocation site there
    // exists a System.exit call.
    Assert.assertEquals(true, backwardQueryResults.isEmpty)
  }

  @Test def killOnSystemExitForwardTest(): Unit = {
    setupSoot(classOf[Nothing])
    val m = Scene.v.getMethod("<boomerang.guided.targets.CustomFlowFunctionTarget: void main(java.lang.String[])>")
    val query = CustomFlowFunctionTest.selectFirstIntAssignment(m)
    val sootCallGraph = new Nothing
    val solver = new Nothing(sootCallGraph, SootDataFlowScope.make(Scene.v), new CustomFlowFunctionTest#CustomBoomerangOptions)
    System.out.println("Solving query: " + query)
    val res = solver.solve(query)
    System.out.println(res.asStatementValWeightTable)
    val t = res.asStatementValWeightTable.cellSet.stream.map((c) => c.getRowKey.getTarget).anyMatch((statement) => statement.containsInvokeExpr && statement.getInvokeExpr.getMethod.getName.equals("queryFor"))
    Assert.assertEquals(false, t)
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
    Options.v.setPhaseOption("cg." + CustomFlowFunctionTest.CG, "on")
    Options.v.setPhaseOption("cg." + CustomFlowFunctionTest.CG, "verbose:true")
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

  private class CustomBoomerangOptions extends Nothing {
    @Override def getForwardFlowFunctions = new Nothing(this)

    @Override def getBackwardFlowFunction = new Nothing(this)
  }

  private class CustomIntAndStringBoomerangOptions extends Nothing {
    @Override def getForwardFlowFunctions = new Nothing(this)

    @Override def getBackwardFlowFunction = new Nothing(this)
  }
}