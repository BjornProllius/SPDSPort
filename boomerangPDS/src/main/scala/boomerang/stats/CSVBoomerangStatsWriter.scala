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
package boomerang.stats

import boomerang.{BackwardQuery,ForwardQuery,Query,Util,WeightedBoomerang}import boomerang.results.{BackwardBoomerangResults,ForwardBoomerangResults}

import boomerang.scene.ControlFlowGraph.Edge import boomerang.scene.{Field,Method,Val}import boomerang.solver.{AbstractBoomerangSolver,ForwardBoomerangSolver}
import com.google.common.base.Joiner import com.google.common.collect.{Lists,Maps,Sets}

import java.io.{File,FileWriter,IOException}
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.mutable import sync.pds.solver.nodes.{GeneratedState,INode,Node}import wpds.impl.{Rule,Transition,Weight}import wpds.interfaces.{Location,State}

class CSVBoomerangStatsWriter[W<:Weight]extends IBoomerangStats[W]
{

  private var queries:Map[Query,AbstractBoomerangSolver[W]]=Maps.newHashMap()
  private var globalFieldTransitions:Set[WeightedTransition[Field,INode[Node[Edge,Val]],W]]=Sets.newHashSet()
  private var fieldTransitionCollisions:Int=0
  private var globalCallTransitions:Set[WeightedTransition[Edge,INode[Val],W]]=Sets.newHashSet()
  private var callTransitionCollisions:Int=0
  private var globalFieldRules:Set[Rule[Field,INode[Node[Edge,Val]],W]]=Sets.newHashSet()
  private var fieldRulesCollisions:Int=0
  private var globalCallRules:Set[Rule[Edge,INode[Val],W]]=Sets.newHashSet()
  private var callRulesCollisions:Int=0
  private var reachedForwardNodes:Set[Node[Edge,Val]]=Sets.newHashSet()
  private var reachedForwardNodeCollisions:Int=0

  private var reachedBackwardNodes:Set[Node[Edge,Val]]=Sets.newHashSet()
  private var reachedBackwardNodeCollisions:Int=0
  private var callVisitedMethods:Set[Method]=Sets.newHashSet()
  private var fieldVisitedMethods:Set[Method]=Sets.newHashSet()
  private var callVisitedStmts:Set[Edge]=Sets.newHashSet()
  private var fieldVisitedStmts:Set[Edge]=Sets.newHashSet()
  private var fieldGeneratedStates:Set[INode[Node[Edge,Val]]]=Sets.newHashSet()
  private var callGeneratedStates:Set[INode[Val]]=Sets.newHashSet()
  private var arrayFlows:Int=0
  private var staticFlows:Int=0
  private var fieldWritePOIs:Int=0
  private var fieldReadPOIs:Int=0

  private var outputFileName:String=_
  private val CSV_SEPARATOR:String=";"
  private var headers:List[String]=Lists.newArrayList()
  private var headersToValues:Map[String,String]=Maps.newHashMap()
  private var memoryBefore:Long=0

  private object Headers extends Enumeration
  {
    type Headers = Value
    val Query, QueryType, FieldTransitions, CallTransitions, CallRules, FieldRules, ReachedForwardNodes, ReachedBackwardNodes, CallVisitedMethods, FieldVisitedMethods, CallVisitedStmts, FieldVisitedStmts, FieldWritePOIs, FieldReadPOIs, StaticFlows, ArrayFlows, QueryTime, Timeout, ICFGEdges, CallGeneratedStates, FieldGeneratedStates, FieldLongestAccessPath, CallLongestCallStack, CallContainsLoop, FieldContainsLoop, MemoryBefore, MemoryAfter, MemoryDiff = Value
  }

  def this(outputFileName:String)
  {
    this()
    this.outputFileName = outputFileName
    for (h <- Headers.values) {
      this.headers.add(h.toString)
    }
    memoryBefore = Util.getReallyUsedMemory
  }

  def sortByValues[K](map:mutable.Map[K,Int]):mutable.Map[K,Int]=
  {
    val sortedByValues = mutable.TreeMap[K, Int]()(new Ordering[K] {

  def compare(k1: K, k2: K): Int = map(k2).compare(map(k1))
    })sortedByValues++=

  map sortedByValues}

  override def

  registerSolver(key: Query, solver: AbstractBoomerangSolver[W]): Unit = {
    if (!queries.contains(key)) {
      queries.put(key, solver)
      solver.getFieldAutomaton.registerListener((t, w, aut) => {
        if (!globalFieldTransitions.add(new WeightedTransition[Field, INode[Node[Edge, Val]], W](t, w))) {
          fieldTransitionCollisions += 1
        }
        fieldVisitedMethods.add(t.getStart.fact.stmt.getMethod)
        fieldVisitedStmts.add(t.getStart.fact.stmt)
        if (t.getLabel.isInstanceOf[ArrayField]) {
          arrayFlows += 1
        }
        addFieldGeneratedState(t.getStart)
        addFieldGeneratedState(t.getTarget)
      })

      solver.getCallAutomaton.registerListener((t, w, aut) => {
        if (!globalCallTransitions.add(new WeightedTransition[Edge, INode[Val], W](t, w))) {
          callTransitionCollisions += 1
        }
        callVisitedMethods.add(t.getLabel.getMethod)
        fieldVisitedStmts.add(t.getLabel)
        if (t.getStart.fact.isStatic) {
          staticFlows += 1
        }
        addCallGeneratedState(t.getStart)
        addCallGeneratedState(t.getTarget)
      })

      solver.getFieldPDS.registerUpdateListener(rule => {
        if (!globalFieldRules.add(rule)) {
          fieldRulesCollisions += 1
        }
      })

      solver.getCallPDS.registerUpdateListener(rule => {
        if (!globalCallRules.add(rule)) {
          callRulesCollisions += 1
        }
      })

      solver.registerListener(reachableNode => {
        if (solver.isInstanceOf[ForwardBoomerangSolver[_]]) {
          if (!reachedForwardNodes.add(reachableNode)) {
            reachedForwardNodeCollisions += 1
          }
        } else {
          if (!reachedBackwardNodes.add(reachableNode)) {
            reachedBackwardNodeCollisions += 1
          }
        }
      })
    }
  }

  protected def addFieldGeneratedState(s:INode[Node[Edge,Val]]):Unit= {
    if(s.isInstanceOf[GeneratedState[_,_]]){fieldGeneratedStates.add(s)}
  }

  protected def addCallGeneratedState(s:INode[Val]):Unit= {
    if(s.isInstanceOf[GeneratedState[_,_]]){callGeneratedStates.add(s)}}

    override def registerFieldWritePOI(key:WeightedBoomerang[W]#FieldWritePOI):Unit={fieldWritePOIs+=1}

    override def toString:String={var s="=========== Boomerang Stats =============\n"var forwardQuery=0 var backwardQuery=0 for(q<-queries.keySet){if(q.isInstanceOf[ForwardQuery]){forwardQuery+=1}else{backwardQuery+=1}}s+=f"Queries (Forward/Backward/Total): \t\t $forwardQuery/$backwardQuery/${queries.keySet.size}\n"s+=f"Visited Methods (Field/Call): \t\t ${fieldVisitedMethods.size}/${callVisitedMethods.size}\n"s+=f"Reached Forward Nodes(Collisions): \t\t ${reachedForwardNodes.size} ($reachedForwardNodeCollisions)\n"s+=f"Reached Backward Nodes(Collisions): \t\t ${reachedBackwardNodes.size} ($reachedBackwardNodeCollisions)\n"s+=f"Global Field Rules(Collisions): \t\t ${globalFieldRules.size} ($fieldRulesCollisions)\n"s+=f"Global Field Transitions(Collisions): \t\t ${globalFieldTransitions.size} ($fieldTransitionCollisions)\n"s+=f"Global Call Rules(Collisions): \t\t ${globalCallRules.size} ($callRulesCollisions)\n"s+=f"Global Call Transitions(Collisions): \t\t ${globalCallTransitions.size} ($callTransitionCollisions)\n"s+=f"Special Flows (Static/Array): \t\t $staticFlows(${globalCallTransitions.size})/$arrayFlows(${globalFieldTransitions.size})\n"s+=computeMetrics()s+="\n"s}

    override def getCallVisitedMethods:Set[Method]={callVisitedMethods.toSet
  }

  private def computeMetrics():String={var min=Int.MaxValue var totalReached=0 var max=0 var maxQuery:Query=null for(q<-queries.keySet){val size=queries(q).getReachedStates.size totalReached+=size min=Math.min(size,min)if(size>max){maxQuery=q}max=Math.max(size,max)}val average=totalReached.toFloat/queries.keySet.size var s=f"Reachable nodes (Min/Avg/Max): \t\t$min/$average/$max\n"s+=f"Maximal Query: \t\t$maxQuery\n"s}

  private class WeightedTransition[X<:Location,Y<:State,W](val t:Transition[X,Y],val w:W){override def hashCode():Int={val prime=31 var result=1 result=prime*result+(if(t==null)0 else t.hashCode)result=prime*result+(if(w==null)0 else w.hashCode)result}

  override def equals(obj:Any):Boolean={obj match{case that:WeightedTransition[X,Y,W]=>(this.t==null&&that.t==null||this.t!=null&&this.t.equals(that.t))&&(this.w==null&&that.w==null||this.w!=null&&this.w.equals(that.w))case _=>false}}}

  override def getForwardReachesNodes:Collection[_<:Node[Edge,Val]]={val res=mutable.HashSet[Node[Edge,Val]]()for(q<-queries.keySet){if(q.isInstanceOf[ForwardQuery])res++=queries(q).getReachedStates}res}

  override def terminated(query:ForwardQuery,res:ForwardBoomerangResults[W]):Unit={writeToFile(query,res.getAnalysisWatch.elapsed(TimeUnit.MILLISECONDS),res.isTimedout)}

  override def terminated(query:BackwardQuery,res:BackwardBoomerangResults[W]):Unit={writeToFile(query,res.getAnalysisWatch.elapsed(TimeUnit.MILLISECONDS),res.isTimedout)}

  private def writeToFile(query:Query,queryTime:Long,timeout:Boolean):Unit={val memoryAfter=Util.getReallyUsedMemory put(Headers.Query,query.toString)put(Headers.QueryType,if(query.isInstanceOf[BackwardQuery])"B"else"F")put(Headers.QueryTime,queryTime)put(Headers.Timeout,if(timeout)"1"else"0")put(Headers.ArrayFlows,arrayFlows)put(Headers.CallRules,globalCallRules.size)put(Headers.FieldRules,globalFieldRules.size)put(Headers.CallTransitions,globalCallTransitions.size)put(Headers.FieldTransitions,globalFieldTransitions.size)put(Headers.FieldReadPOIs,fieldReadPOIs)put(Headers.FieldWritePOIs,fieldWritePOIs)put(Headers.FieldVisitedMethods,fieldVisitedMethods.size)put(Headers.CallVisitedMethods,callVisitedMethods.size)put(Headers.FieldVisitedStmts,fieldVisitedStmts.size)put(Headers.CallVisitedStmts,callVisitedStmts.size)put(Headers.ReachedForwardNodes,reachedForwardNodes.size)put(Headers.ReachedBackwardNodes,reachedBackwardNodes.size)put(Headers.StaticFlows,staticFlows)
  // TODO implement
  // put(Headers.ICFGEdges, Util.getICFGEdges())
  put(Headers.CallGeneratedStates,callGeneratedStates.size)put(Headers.FieldGeneratedStates,fieldGeneratedStates.size)put(Headers.CallLongestCallStack,queries(query).getCallAutomaton.getLongestPath.size)put(Headers.FieldLongestAccessPath,queries(query).getFieldAutomaton.getLongestPath.size)put(Headers.CallContainsLoop,queries(query).getCallAutomaton.containsLoop)put(Headers.FieldContainsLoop,queries(query).getFieldAutomaton.containsLoop)put(Headers.MemoryAfter,memoryAfter)put(Headers.MemoryBefore,memoryBefore)put(Headers.MemoryDiff,memoryAfter-memoryBefore)

  try{val reportFile=new File(outputFileName).getAbsoluteFile if(!reportFile.getParentFile.exists){try{Files.createDirectories(reportFile.getParentFile.toPath)}catch{case e:IOException=>throw new RuntimeException("Was not able to create directories for IDEViz output!")}}val fileExisted=reportFile.exists val writer=new FileWriter(reportFile,true)if(!fileExisted){writer.write(headers.mkString(CSV_SEPARATOR)+"\n")}val line=headers.map(h=>Option(headersToValues.get(h)).getOrElse(""))writer.write(line.mkString(CSV_SEPARATOR)+"\n")writer.close()}catch{case e:IOException=>e.printStackTrace()}}

  private def put(key:String,val:Any):Unit={if(!headers.contains(key)){System.err.println("Did not create a header to this value "+key)}else{headersToValues.put(key,val.toString)}}

  private def put(key:Headers,val:Any):Unit={put(key.toString,val)}
}