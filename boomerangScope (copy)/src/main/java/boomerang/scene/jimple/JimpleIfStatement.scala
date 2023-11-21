package boomerang.scene.jimple

import boomerang.scene.IfStatement
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import soot.Value
import soot.jimple.ConditionExpr
import soot.jimple.EqExpr
import soot.jimple.IfStmt
import soot.jimple.IntConstant
import soot.jimple.NeExpr
import soot.jimple.NullConstant

class JimpleIfStatement(private var delegate: Nothing, private var method: Nothing) extends Nothing {
  @Override def getTarget: Nothing = JimpleStatement.create(delegate.getTarget, method.asInstanceOf[Nothing])

  @Override def evaluate(`val`: Nothing): Nothing = {
    if (delegate.getCondition.isInstanceOf[Nothing]) {
      val eqExpr = delegate.getCondition.asInstanceOf[Nothing]
      val op1 = eqExpr.getOp1
      val op2 = eqExpr.getOp2
      if (`val`.equals(new Nothing(op1, method)) && op2.equals(NullConstant.v) || (`val`.equals(new Nothing(op2, method)) && op2.equals(NullConstant.v))) return Evaluation.TRUE
      if (`val`.equals(new Nothing(IntConstant.v(0), method)) && op2.equals(IntConstant.v(0)) || (`val`.equals(new Nothing(IntConstant.v(1), method)) && op2.equals(IntConstant.v(1)))) return Evaluation.TRUE
      if (`val`.equals(new Nothing(IntConstant.v(1), method)) && op2.equals(IntConstant.v(0)) || (`val`.equals(new Nothing(IntConstant.v(0), method)) && op2.equals(IntConstant.v(1)))) return Evaluation.FALSE
    }
    if (delegate.getCondition.isInstanceOf[Nothing]) {
      val eqExpr = delegate.getCondition.asInstanceOf[Nothing]
      val op1 = eqExpr.getOp1
      val op2 = eqExpr.getOp2
      if (`val`.equals(new Nothing(op1, method)) && op2.equals(NullConstant.v) || (`val`.equals(new Nothing(op2, method)) && op2.equals(NullConstant.v))) return Evaluation.FALSE
      if (`val`.equals(new Nothing(IntConstant.v(0), method)) && op2.equals(IntConstant.v(0)) || (`val`.equals(new Nothing(IntConstant.v(1), method)) && op2.equals(IntConstant.v(1)))) return Evaluation.FALSE
      if (`val`.equals(new Nothing(IntConstant.v(1), method)) && op2.equals(IntConstant.v(0)) || (`val`.equals(new Nothing(IntConstant.v(0), method)) && op2.equals(IntConstant.v(1)))) return Evaluation.TRUE
    }
    Evaluation.UNKOWN
  }

  @Override def uses(`val`: Nothing): Boolean = {
    if (delegate.getCondition.isInstanceOf[Nothing]) {
      val c = delegate.getCondition.asInstanceOf[Nothing]
      val op1 = c.getOp1
      val op2 = c.getOp2
      return `val`.equals(new Nothing(op1, method)) || `val`.equals(new Nothing(op2, method))
    }
    false
  }
}