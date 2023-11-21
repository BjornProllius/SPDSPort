package boomerang.scene.jimple

import com.google.common.collect.Sets
import java.util
import soot.RefType
import soot.Scene
import soot.SootClass
import soot.SootField
import soot.SootMethod
import soot.Unit
import soot.UnitPatchingChain
import soot.Value
import soot.jimple.AssignStmt
import soot.jimple.InstanceFieldRef
import soot.jimple.NullConstant
import soot.jimple.internal.JAssignStmt
import soot.jimple.internal.JInstanceFieldRef
import soot.util.Chain

object ExplicitNullifyFields {
  def apply(): Unit = {
    import scala.collection.JavaConversions._
    for (c <- Scene.v.getClasses) {
      import scala.collection.JavaConversions._
      for (m <- c.getMethods) {
        if (m.hasActiveBody && m.isConstructor) apply(m)
      }
    }
  }

  private def apply(cons: Nothing): Unit = {
    val fields = cons.getDeclaringClass.getFields
    val units = cons.getActiveBody.getUnits
    val fieldsDefinedInMethod = getFieldsDefinedInMethod(cons)
    import scala.collection.JavaConversions._
    for (f <- fields) {
      if (fieldsDefinedInMethod.contains(f)) continue //todo: continue is not supported
      if (f.isStatic) continue //todo: continue is not supported
      if (f.isFinal) continue //todo: continue is not supported
      if (f.getType.isInstanceOf[Nothing]) units.addFirst(new Nothing(new Nothing(cons.getActiveBody.getThisLocal, f.makeRef), NullConstant.v))
    }
  }

  private def getFieldsDefinedInMethod(cons: Nothing) = {
    val res = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (u <- cons.getActiveBody.getUnits) {
      if (u.isInstanceOf[Nothing]) {
        val as = u.asInstanceOf[Nothing]
        val left = as.getLeftOp
        if (left.isInstanceOf[Nothing]) {
          val ifr = left.asInstanceOf[Nothing]
          res.add(ifr.getField)
        }
      }
    }
    res
  }
}