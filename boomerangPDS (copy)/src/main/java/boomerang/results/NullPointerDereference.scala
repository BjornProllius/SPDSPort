package boomerang.results

import boomerang.Query
import boomerang.scene.ControlFlowGraph
import boomerang.scene.Field
import boomerang.scene.Method
import boomerang.scene.Pair
import boomerang.scene.Statement
import boomerang.scene.Val
import java.util
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.PAutomaton

object NullPointerDereference {
  val RULE_INDEX = 0

  def isNullPointerNode(nullPointerNode: Nothing): Boolean = {
    val fact = nullPointerNode.fact
    val m = fact.m
    // A this variable can never be null.
    if (!m.isStatic && m.getThisLocal.equals(fact)) return false
    val curr = nullPointerNode.stmt.getStart
    if (curr.containsInvokeExpr) if (curr.getInvokeExpr.isInstanceInvokeExpr) {
      val invocationBase = curr.getInvokeExpr.getBase
      if (invocationBase.equals(fact)) return true
    }
    if (curr.isAssign) {
      if (curr.isFieldLoad) {
        val ifr = curr.getFieldLoad
        if (ifr.getX.equals(fact)) return true
      }
      if (curr.getRightOp.isLengthExpr) {
        val lengthOp = curr.getRightOp.getLengthOp
        if (lengthOp.equals(fact)) return true
      }
    }
    false
  }
}

class NullPointerDereference(private var query: Nothing, private val statement: Nothing, private val variable: Nothing, private var openingContext: Nothing, private var closingContext: Nothing, private var dataFlowPath: Nothing) extends Nothing {
  this.sourceStatement = query.cfgEdge
  this.sourceVariable = query.`var`
  final private var sourceStatement: Nothing = null
  final private var sourceVariable: Nothing = null

  def this(statement: Nothing) {
    this(null, statement, null, null, null, null)
  }

  /**
   * The variable that contains "null" and which provokes at {@link # getStatement ( ) the statement} a
   * NullPointerException.
   *
   * @return the variable that contains a null pointer
   */
  def getVariable: Nothing = variable

  @Override def getDataFlowPath: Nothing = dataFlowPath

  @Override def getMessage = "Potential **null pointer** dereference"

  @Override def getRuleIndex: Int = NullPointerDereference.RULE_INDEX

  /**
   * The statement at which a null pointer occurred.
   *
   * <p>A null pointer can occur at three different types of statements: y = x.toString(); or y =
   * lengthof(x); or y = x.f;
   *
   * @return the statement where the respective {@link # getVariable ( ) getVariable} is null
   */
  def getStatement: Nothing = statement

  /**
   * The source statement of the data-flow, i.e., the statement that assigns null to a variable.
   *
   * <p>Examples are: x = null or x = System.getProperty(...).
   *
   * @return The source statement of the data-flow/null pointer.
   */
  def getSourceStatement: Nothing = sourceStatement

  /**
   * The source variable at the source statement. At a statement x = null or x = System.getProperty,
   * this will be the variable x.
   *
   * @return The source variable of the data-flow propagation
   */
  def getSourceVariable: Nothing = sourceVariable

  /**
   * Returns the method of the statement at which the null pointer occurs.
   *
   * @return The SootMethod of the null pointer statement
   */
  def getMethod: Nothing = getStatement.getStart.getMethod

  /**
   * The opening context of a NullPointer provides the call stack under which the null pointer
   * occurs.
   *
   * <pre>
   * main(){
   * Object x = null;
   * foo(x); //call site context "c1"
   * Object y = new Object();
   * foo(y); //call site context "c2"
   * }
   * foo(Object z){
   * z.toString() //<- Variable z is null here under context c1, but *not* under c2)
   * }
   * </pre>
   *
   * In the example above, z is null under the calling context of call site c1.
   *
   * <p>In the case of branching, there can be multiple call site contexts leading to a null
   * pointer. Therefore, the opening context is represented as an automaton (or graph). The edges of
   * the automaton are labeled by the call sites, the nodes are labeled by variables or by variables
   * at a context. For the example above, the automaton contains a transition with label foo(x)
   *
   * @return The automaton representation of the opening context.
   */
  def getOpeningContext: Nothing = openingContext

  /**
   * The closing context of a NullPointer provides the call stack via which a variable containing
   * null returns to a caller.
   *
   * <pre>
   * main(){
   * Object x;
   * if(...){
   * x = returnNull(); //b1
   * } else {
   * x = returnNotNull(); //b2
   * }
   * x.toString() //<- Variable x is null here when the program executes along branch b1
   * }
   * Object returnNull(){
   * Object y = null;
   * return y;
   * }
   * </pre>
   *
   * In the case above, a null pointer exception occurs when the program executes along branch b1.
   *
   * <p>There can be multiple contexts leading to a null pointer. Therefore, the closing context is
   * represented as an automaton (or graph). The edges of the automaton are labeled by the call
   * sites, the nodes are labeled by variables or by variables at a context. For the example above,
   * the automaton contains a transition with label returnNull(). This indicates, that the null
   * pointer only occurs along branch b1 but not b2.
   *
   * @return The automaton representation of the closing context.
   */
  def getClosingContext: Nothing = closingContext

  @Override def toString: Nothing = {
    var str = "Null Pointer: \n"
    str += "defined at " + getSourceStatement.getStart.getMethod
    str += (if (getVariable != null) "\tVariable: " + getVariable
    else "")
    str += "\n\tStatement: " + getStatement + "\n\tMethod: " + getMethod
    str
  }

  def getQuery: Nothing = query
}