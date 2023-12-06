/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package test.core.selfrunning

import java.io.{File,IOException}

import java.nio.file.Files import java.util.{Collections,LinkedList,List}import org.junit.{Assert,Before,Rule}
import org.junit.rules.TestName
import soot._ import soot.jimple.{Jimple,JimpleBody}
import soot.options.Options

import org.junit.{Before,Rule}
import org.junit.rules.TestName import soot.{PackManager,SootMethod,Transform}

abstract class AbstractTestingFramework {
  @Rule
  val testMethodName:TestName=new TestName()
  protected var sootTestMethod:SootMethod=_
  protected var ideVizFile:File=_
  protected var dotFile:File=_

  @Before
  def beforeTestCaseExecution(): Unit = {
    initializeSootWithEntryPoint()
    createDebugFiles()
    try {
      analyze()
    } catch {
      case e: ImprecisionException => Assert.fail(e.getMessage)
    }
    org.junit.Assume.assumeTrue(false)
  }

  private def createDebugFiles(): Unit = {
    ideVizFile = new File("target/IDEViz/" + getTestCaseClassName() + "/IDEViz-" + testMethodName.getMethodName() + ".json")
    if (!ideVizFile.getParentFile.exists()) {
      try {
        Files.createDirectories(ideVizFile.getParentFile.toPath)
      } catch {
        case e: IOException => throw new RuntimeException("Was not able to create directories for IDEViz output!")
      }
    }
    dotFile = new File("target/dot/" + getTestCaseClassName() + "/Dot-" + testMethodName.getMethodName() + ".dot")
    if (!dotFile.getParentFile.exists()) {
      try {
        Files.createDirectories(dotFile.getParentFile.toPath)
      } catch {
        case e: IOException => throw new RuntimeException("Was not able to create directories for dot output!")
      }
    }
  }

  private def analyze(): Unit = {
    val transform = new Transform("wjtp.ifds", createAnalysisTransformer())
    PackManager.v().getPack("wjtp").add(transform)
    PackManager.v().getPack("cg").apply()
    PackManager.v().getPack("wjtp").apply()
  }

  protected def createAnalysisTransformer(): SceneTransformer

  private def initializeSootWithEntryPoint(): Unit = {
    G.v().reset()
    Options.v().set_whole_program(true)

    Options.v().setPhaseOption("cg.spark", "on")
    Options.v().setPhaseOption("cg.spark", "verbose:true")
    Options.v().set_output_format(Options.output_format_none)

    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)

    Options.v().set_include(getIncludeList())

    Options.v().setPhaseOption("jb", "use-original-names:true")

    Options.v().set_exclude(excludedPackages())

    if (getJavaVersion() < 9) {
      Options.v().set_prepend_classpath(true)
      Options.v().set_soot_classpath(getSootClassPath())
    } else if (getJavaVersion() >= 9) {
      Options.v().set_soot_classpath("VIRTUAL_FS_FOR_JDK" + File.pathSeparator + getSootClassPath())
    }

    val sootTestCaseClass = Scene.v().forceResolve(getTestCaseClassName(), SootClass.BODIES)

    for (m <- sootTestCaseClass.getMethods) {
      if (m.getName.equals(testMethodName.getMethodName)) sootTestMethod = m
    }

    if (sootTestMethod == null)
      throw new RuntimeException(
        "The method with name "
          + testMethodName.getMethodName()
          + " was not found in the Soot Scene.")

    sootTestMethod.getDeclaringClass().setApplicationClass()
    Scene.v().addBasicClass(getTargetClass(), SootClass.BODIES)
    Scene.v().loadNecessaryClasses()
    val c = Scene.v().forceResolve(getTargetClass(), SootClass.BODIES)
    if (c != null) {
      c.setApplicationClass()
    }
    val methodByName = c.getMethodByName("main")
    val ePoints = new LinkedList[SootMethod]()
    for (m <- sootTestCaseClass.getMethods) {
      if (m.isStaticInitializer()) ePoints.add(m)
    }
    for (inner <- Scene.v().getClasses) {
      if (inner.getName.contains(sootTestCaseClass.getName)) {
        inner.setApplicationClass()
        for (m <- inner.getMethods) {
          if (m.isStaticInitializer()) ePoints.add(m)
        }
      }
    }
    ePoints.add(methodByName)
    Scene.v().setEntryPoints(ePoints)
  }

  protected def getSootClassPath(): String = {
    val userdir = System.getProperty("user.dir")
    val javaHome = System.getProperty("java.home")
    if (javaHome == null || javaHome.equals(""))
      throw new RuntimeException("Could not get property java.home!")

    var sootCp = userdir + "/target/test-classes"
    if (getJavaVersion() < 9) {
      sootCp += File.pathSeparator + javaHome + "/lib/rt.jar"
      sootCp += File.pathSeparator + javaHome + "/lib/jce.jar"
    }
    sootCp
  }

  protected def getIncludeList():List[String]= {
    val includeList=new LinkedList[String]()includeList.add("java.lang.*")includeList.add("java.util.*")includeList.add("java.io.*")includeList.add("sun.misc.*")includeList.add("java.net.*")includeList.add("sun.nio.*")includeList.add("javax.servlet.*")includeList
  }

  private def getTargetClass():String= {
    val sootClass=new SootClass("dummyClass")val paramType=ArrayType.v(RefType.v("java.lang.String"),1)val mainMethod=new SootMethod("main",Collections.singletonList(paramType),VoidType.v(),Modifier.PUBLIC|Modifier.STATIC)sootClass.addMethod(mainMethod)val body=Jimple.v().newBody(mainMethod)mainMethod.setActiveBody(body)val testCaseType=RefType.v(getTestCaseClassName())val loc=Jimple.v().newLocal("l0",paramType)body.getLocals().add(loc)body.getUnits().add(Jimple.v().newIdentityStmt(loc,Jimple.v().newParameterRef(paramType,0)))val allocatedTestObj=Jimple.v().newLocal("dummyObj",testCaseType)body.getLocals().add(allocatedTestObj)body.getUnits().add(Jimple.v().newAssignStmt(allocatedTestObj,Jimple.v().newNewExpr(testCaseType)))body.getUnits().add(Jimple.v().newInvokeStmt(Jimple.v().newVirtualInvokeExpr(allocatedTestObj,sootTestMethod.makeRef())))body.getUnits().add(Jimple.v().newReturnVoidStmt())

    Scene.v().addClass(sootClass)body.validate()sootClass.toString()
  }

  def getTestCaseClassName():String= {
    this.getClass().getName().replace("class ","")
  }

  def excludedPackages():List[String]= {
    val excludedPackages=new LinkedList[String]()excludedPackages.add("sun.*")excludedPackages.add("javax.*")excludedPackages.add("com.sun.*")excludedPackages.add("com.ibm.*")excludedPackages.add("org.xml.*")excludedPackages.add("org.w3c.*")excludedPackages.add("apple.awt.*")excludedPackages.add("com.apple.*")excludedPackages
  }

  protected def staticallyUnknown():Boolean= {
    true
  }

  private def getJavaVersion():Int= {
    var version=System.getProperty("java.version")if(version.startsWith("1.")){version=version.substring(2,3)}else{val dot=version.indexOf(".")if(dot!=-1){version=version.substring(0,dot)}}Integer.parseInt(version)}
}