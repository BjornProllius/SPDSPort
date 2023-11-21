package boomerang

import java.lang.management.GarbageCollectorMXBean
import java.lang.management.ManagementFactory

object Util {
  private val icfgEdges = 0

  private def getGcCount = {
    var sum = 0
    import scala.collection.JavaConversions._
    for (b <- ManagementFactory.getGarbageCollectorMXBeans) {
      val count = b.getCollectionCount
      if (count != -1) sum += count
    }
    sum
  }

  //    public static long getICFGEdges() {
  //        if (icfgEdges > 0)
  //            return icfgEdges;
  //        ReachableMethods reachableMethods = Scene.v().getReachableMethods();
  //        JimpleBasedInterproceduralCFG icfg = new JimpleBasedInterproceduralCFG();
  //        QueueReader<MethodOrMethodContext> listener = reachableMethods.listener();
  //        while (listener.hasNext()) {
  //            MethodOrMethodContext next = listener.next();
  //            SootMethod method = next.method();
  //            if (!method.hasActiveBody())
  //                continue;
  //            Body activeBody = method.getActiveBody();
  //            for (Unit u : activeBody.getUnits()) {
  //                List<Unit> succsOf = icfg.getSuccsOf(u);
  //                icfgEdges += succsOf.size();
  //                if (icfg.isCallStmt(u)) {
  //                    icfgEdges += icfg.getCalleesOfCallAt(u).size();
  //                }
  //                if (icfg.isExitStmt(u)) {
  //                    icfgEdges += icfg.getCallersOf(method).size();
  //                }
  //            }
  //        }
  //        return icfgEdges;
  //    }
  def getReallyUsedMemory: Long = {
    0
    // long before = getGcCount();
    // System.gc();
    // while (getGcCount() == before)
    // ;
    // return getCurrentlyUsedMemory();
  }

  private def getCurrentlyUsedMemory = {
    ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
    // + ManagementFactory.getMemoryMXBean().getNonHeapMemoryUsage().getUsed();
  }
}