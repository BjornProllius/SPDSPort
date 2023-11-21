package test.cases.bugfixes

import boomerang.Boomerang
import boomerang.BoomerangOptions
import boomerang.ForwardQuery
import boomerang.results.ForwardBoomerangResults
import boomerang.scene._
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.jimple._
import com.google.common.collect.Sets
import java.util
import java.util.Collections
import java.util.Map.Entry
import org.junit.Test
import soot._
import soot.options.Options
import wpds.impl.Weight.NoWeight

/**
 * This code was added to test https://github.com/CodeShield-Security/SPDS/issues/5.
 * Thanks @copumpkin for sharing code for testing purpose.
 */
object Repro {
  private[bugfixes] val opts = new Nothing

  private def setupSoot(classPath: Nothing, excludeFoo: Boolean): Unit = {
    Options.v.set_whole_program(true)
    Options.v.setPhaseOption("cg.spark", "on")
    Options.v.set_no_bodies_for_excluded(true)
    Options.v.set_allow_phantom_refs(true)
    Options.v.set_keep_line_number(true)
    /* ********* Uncomment this line to see methods invoked on Foo! ********* */
    if (excludeFoo) Options.v.set_exclude(Collections.singletonList("Foo"))
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.set_soot_classpath(classPath)
    Options.v.set_prepend_classpath(true)
    Options.v.set_process_dir(Arrays.asList(classPath.split(":")))
    Scene.v.loadNecessaryClasses
  }

  private def analyze(expectedCallSignatureOnFoo: Nothing*): Unit = {
    PackManager.v.getPack("wjtp").add(new Nothing("wjtp.repro", new Repro.ReproTransformer(expectedCallSignatureOnFoo)))
    PackManager.v.getPack("cg").apply
    PackManager.v.getPack("wjtp").apply
  }

  private def getMethodsInvokedFromInstanceInStatement(queryStatement: Nothing) = {
    val `var` = new Nothing(queryStatement.getLeftOp, queryStatement, queryStatement.getRightOp)
    val fwq = new Nothing(new Nothing(queryStatement, queryStatement.getMethod.getControlFlowGraph.getSuccsOf(queryStatement).stream.findFirst.get), `var`)
    val solver = new Nothing(new Nothing, SootDataFlowScope.make(Scene.v), opts)
    val results = solver.solve(fwq)
    results.getInvokedMethodOnInstance
  }

  private[bugfixes] class ReproTransformer(expectedCallSignatureOnFoo: Nothing*) extends Nothing {
    expectedCalledMethodsOnFoo = Sets.newHashSet(expectedCallSignatureOnFoo)
    private[bugfixes] val expectedCalledMethodsOnFoo: Nothing = null

    @Override protected def internalTransform(name: Nothing, options: Nothing): Unit = {
      BoomerangPretransformer.v.reset
      BoomerangPretransformer.v.apply
      val m = Scene.v.getMethod("<Test: java.util.List foos()>")
      val method = JimpleMethod.of(m)
      System.out.println("All method units:")
      import scala.collection.JavaConversions._
      for (s <- method.getControlFlowGraph.getStatements) {
        System.out.println("\t" + s.toString)
      }
      val newFoo = method.getControlFlowGraph.getStatements.stream.filter((x) => x.toString.contains("$stack2 = new Foo")).findFirst.get
      // This will only show results if set_exclude above gets uncommented
      System.out.println("\nFoo invoked methods:")
      val entries = getMethodsInvokedFromInstanceInStatement(newFoo).entrySet
      val methodCalledOnFoo = Sets.newHashSet
      import scala.collection.JavaConversions._
      for (e <- entries) {
        System.out.println("\t" + e.getKey.toString)
        System.out.println("\t\t" + e.getValue.toString)
        methodCalledOnFoo.add(e.getValue.toString)
      }
      assert(methodCalledOnFoo.equals(Sets.newHashSet(expectedCalledMethodsOnFoo)))
    }
  }
}

class Repro {
  @Test def excludeFoo(): Unit = {
    G.reset
    Repro.setupSoot("src/test/resources/Test.jar", true)
    Repro.analyze("<Foo: void baz()>", "<Foo: void bar()>", "<Foo: void <init>()>")
  }

  @Test def includeFoo(): Unit = {
    G.reset
    Repro.setupSoot("src/test/resources/Test.jar", false)
    Repro.analyze("<Foo: void baz()>", "<Foo: void bar()>", "<Foo: void <init>()>", "<java.lang.Object: void <init>()>")
  }
}