/**
 * ***************************************************************************** Copyright (c) 2020
 * CodeShield GmbH, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang.scene.wala

import boomerang.scene.CallGraph
import com.google.common.base.Stopwatch
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import com.ibm.wala.classLoader.CallSiteReference
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import java.util
import java.util.concurrent.TimeUnit

class WALACallGraph(cg: Nothing, private var cha: Nothing) extends Nothing {
  val ep: Nothing = cg.getEntrypointNodes
  val visited: Nothing = Sets.newHashSet
  val worklist: Nothing = Lists.newLinkedList

  import scala.collection.JavaConversions._

  for (e <- ep) {
    worklist.add(e)
    this.addEntryPoint(getOrCreate(e))
  }
  val watch: Nothing = Stopwatch.createStarted
  val irw: Nothing = Stopwatch.createUnstarted
  while (!worklist.isEmpty) {
    val curr = worklist.poll
    if (ignore(strongUpdateNode.getMethod)) continue //todo: continue is not supported
    if (!visited.add(getOrCreate(strongUpdateNode))) continue //todo: continue is not supported
    val succNodes = cg.getSuccNodes(strongUpdateNode)
    while (succNodes.hasNext) {
      val succ = succNodes.next
      irw.start
      irw.stop
      //				if(visited.size() % 100 == 0) {
      //					System.out.println("Total Time:" +watch.elapsed(TimeUnit.SECONDS));
      //					System.out.println("IR time: " + irw.elapsed(TimeUnit.SECONDS));
      //					System.out.println(visited.size());
      //					System.out.println(worklist.size());
      //				}
      worklist.add(succ)
      val callSites = cg.getPossibleSites(strongUpdateNode, succ)
      while (callSites.hasNext) {
        val ref = callSites.next
        val calls = strongUpdateNode.getIR.getCalls(ref)
        for (i <- calls) {
          if (ignore(succ.getMethod)) continue //todo: continue is not supported
          this.addEdge(new Nothing(new Nothing(i, getOrCreate(strongUpdateNode)), getOrCreate(succ)))
        }
      }
    }
  }
  System.out.println("Edges:" + size)
  System.out.println("Total Time:" + watch.elapsed(TimeUnit.SECONDS))
  System.out.println("IR time: " + irw.elapsed(TimeUnit.SECONDS))
  private val iMethodToWALAMethod = new Nothing

  private def getOrCreate(curr: Nothing): Nothing = {
    val method = curr.getMethod
    val walaMethod = iMethodToWALAMethod.get(method)
    if (walaMethod != null) return walaMethod
    val m = new Nothing(curr.getMethod, curr.getIR, cha)
    iMethodToWALAMethod.put(method, m)
    m
  }

  private def ignore(method: Nothing) = method.isBridge || method.isClinit || method.isNative || method.isSynthetic || method.isWalaSynthetic
}