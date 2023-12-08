package boomerang.guided

import boomerang.{BackwardQuery,Boomerang,DefaultBoomerangOptions,ForwardQuery}import boomerang.flowfunction.{IBackwardFlowFunction,IForwardFlowFunction}import boomerang.guided.flowfunction.{CustomBackwardFlowFunction,CustomForwardFlowFunction}

import boomerang.guided.targets.CustomFlowFunctionTarget import boomerang.results.{BackwardBoomerangResults,ForwardBoomerangResults}import boomerang.scene.{AllocVal,ControlFlowGraph,Method,SootDataFlowScope,Statement,Val}import boomerang.scene.jimple.{BoomerangPretransformer,IntAndStringBoomerangOptions,JimpleMethod,SootCallGraph}
import boomerang.solver.BackwardBoomerangSolver
import com.google.common.collect.Lists import java.nio.file.{Path,Paths}
import java.util.List import org.junit.{Assert,Test}import soot.{G,PackManager,Scene,SootClass,SootMethod}
import soot.options.Options
import wpds.impl.Weight.NoWeight

class CustomFlowFunctionTest {

  val CG:String="cha"

  @Test
  def killOnSystemExitBackwardTestInteger(): Unit = {
    setupSoot(classOf[CustomFlowFunctionTarget])
    val m: SootMethod = Scene.v().getMethod("<boomerang.guided.targets.CustomFlowFunctionIntTarget: void main(java.lang.String[])>")
    val query: BackwardQuery = selectQueryForStatement(m)

    val sootCallGraph: SootCallGraph = new SootCallGraph()
    val solver: Boomerang = new Boomerang(sootCallGraph, SootDataFlowScope.make(Scene.v()), new CustomIntAndStringBoomerangOptions())

    println("Solving query: " + query)
    val backwardQueryResults: BackwardBoomerangResults[NoWeight] = solver.solve(query)
    for (bw <- solver.getBackwardSolvers().values()) {
      Assert.assertEquals(true, bw.getCallAutomaton().getTransitions().size() < 3)
    }
    println(backwardQueryResults.getAllocationSites())

    // For the query no allocation site is found, as between queryFor and the allocation site there
    // exists a System.exit call.
    Assert.assertEquals(true, backwardQueryResults.isEmpty())
  }

  @Test
  def killOnSystemExitBackwardTest(): Unit = {
    setupSoot(classOf[CustomFlowFunctionTarget])
    val m: SootMethod = Scene.v().getMethod("<boomerang.guided.targets.CustomFlowFunctionTarget: void main(java.lang.String[])>")
    val query: BackwardQuery = selectQueryForStatement(m)

    val sootCallGraph: SootCallGraph = new SootCallGraph()
    val solver: Boomerang = new Boomerang(sootCallGraph, SootDataFlowScope.make(Scene.v()), new CustomBoomerangOptions())

    println("Solving query: " + query)
    val backwardQueryResults: BackwardBoomerangResults[NoWeight] = solver.solve(query)
    println(backwardQueryResults.getAllocationSites())

    // For the query no allocation site is found, as between queryFor and the allocation site there
    // exists a System.exit call.
    Assert.assertEquals(true, backwardQueryResults.isEmpty())
  }

  @Test
  def killOnSystemExitForwardTest(): Unit = {
    setupSoot(classOf[CustomFlowFunctionTarget])
    val m: SootMethod = Scene.v().getMethod("<boomerang.guided.targets.CustomFlowFunctionTarget: void main(java.lang.String[])>")
    val query: ForwardQuery = selectFirstIntAssignment(m)

    val sootCallGraph: SootCallGraph = new SootCallGraph()
    val solver: Boomerang = new Boomerang(sootCallGraph, SootDataFlowScope.make(Scene.v()), new CustomBoomerangOptions())

    println("Solving query: " + query)
    val res: ForwardBoomerangResults[NoWeight] = solver.solve(query)
    println(res.asStatementValWeightTable())

    val t: Boolean = res.asStatementValWeightTable().cellSet().stream()
      .map(c => c.getRowKey().getTarget())
      .anyMatch(
        statement =>
          statement.containsInvokeExpr()
            && statement.getInvokeExpr().getMethod().getName().equals("queryFor"))
    Assert.assertEquals(false, t)
  }

  def selectQueryForStatement(m: SootMethod): BackwardQuery = {
    val method: Method = JimpleMethod.of(m)
    method.getStatements().stream().filter(x => x.containsInvokeExpr()).forEach(x => x.toString())
    val queryStatement: Statement = method.getStatements().stream()
      .filter(x => x.containsInvokeExpr())
      .filter(x => x.getInvokeExpr().getMethod().getName().equals("queryFor"))
      .findFirst()
      .get()
    val arg: Val = queryStatement.getInvokeExpr().getArg(0)

    val predecessor: Statement = method.getControlFlowGraph().getPredsOf(queryStatement).stream().findFirst().get()
    val cfgEdge: Edge = new Edge(predecessor, queryStatement)
    BackwardQuery.make(cfgEdge, arg)
  }

  def selectFirstIntAssignment(m:SootMethod):ForwardQuery= {
    val method:Method=JimpleMethod.of(m)method.getStatements().stream().forEach(x=>println(x.toString()))val intAssignStmt:Statement=method.getStatements().stream().filter(x=>x.isAssign()&&!x.getLeftOp().getType().isRefType()).findFirst().get()val arg:Val=new AllocVal(intAssignStmt.getLeftOp(),intAssignStmt,intAssignStmt.getRightOp())

    val succs:Statement=method.getControlFlowGraph().getSuccsOf(intAssignStmt).stream().findFirst().get()val cfgEdge:Edge=new Edge(intAssignStmt,succs)new ForwardQuery(cfgEdge,arg)
  }

  protected def setupSoot(cls:Class[_]):Unit= {
    G.v().reset()setupSoot()setApplicationClass(cls)PackManager.v().runPacks()BoomerangPretransformer.v().reset()BoomerangPretransformer.v().apply()
  }

  private def setupSoot():Unit= {
    Options.v().set_whole_program(true)Options.v().setPhaseOption("cg."+CG,"on")Options.v().setPhaseOption("cg."+CG,"verbose:true")Options.v().set_output_format(Options.output_format_none)Options.v().set_no_bodies_for_excluded(true)Options.v().set_allow_phantom_refs(true)Options.v().setPhaseOption("jb","use-original-names:true")Options.v().set_keep_line_number(true)Options.v().set_prepend_classpath(true)Options.v().set_process_dir(getProcessDir())
  }

  private def setApplicationClass(cls:Class[_]):Unit= {
    Scene.v().loadNecessaryClasses()val eps=Lists.newArrayList[SootMethod]()for(sootClass<-Scene.v().getClasses()){if(sootClass.toString().equals(cls.getName())||(sootClass.toString().contains(cls.getName()+"$"))){sootClass.setApplicationClass()eps.addAll(sootClass.getMethods())}}Scene.v().setEntryPoints(eps)
  }

  private def getProcessDir():List[String]={val path=Paths.get("target/test-classes")Lists.newArrayList(path.toAbsolutePath().toString())}

  private class CustomBoomerangOptions extends DefaultBoomerangOptions{

  override def getForwardFlowFunctions():IForwardFlowFunction={new CustomForwardFlowFunction(this)}

  override def getBackwardFlowFunction():IBackwardFlowFunction={new CustomBackwardFlowFunction(this)}}

  private class CustomIntAndStringBoomerangOptions extends IntAndStringBoomerangOptions{

  override def getForwardFlowFunctions():IForwardFlowFunction={new CustomForwardFlowFunction(this)}

  override def getBackwardFlowFunction():IBackwardFlowFunction={new CustomBackwardFlowFunction(this)}}
}