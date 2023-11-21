package typestate.tests

import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.FileMustBeClosedStateMachine
import typestate.test.helper.File

object FileMustBeClosedTestDemandDriven {
  private trait I {
    def flow(f: Nothing): Unit
  }

  private class B extends FileMustBeClosedTestDemandDriven.I {
    @Override override def flow(f: Nothing): Unit = {
      f.close
    }
  }

  private class A extends FileMustBeClosedTestDemandDriven.I {
    @Override override def flow(f: Nothing): Unit = {
      shouldNotBeAnalyzed
    }
  }
}

class FileMustBeClosedTestDemandDriven extends Nothing {
  @Test def notCaughtByCHA(): Unit = {
    val b = new FileMustBeClosedTestDemandDriven.B
    callOnInterface(b)
  }

  private def callOnInterface(i: FileMustBeClosedTestDemandDriven.I): Unit = {
    val file = new Nothing
    file.open
    i.flow(file)
    mustBeInAcceptingState(file)
  }

  @Test def notCaughtByRTA(): Unit = {
    val a = new FileMustBeClosedTestDemandDriven.A
    val b = new FileMustBeClosedTestDemandDriven.B
    callOnInterface(b)
  }

  @Override protected def getStateMachine = new Nothing
}