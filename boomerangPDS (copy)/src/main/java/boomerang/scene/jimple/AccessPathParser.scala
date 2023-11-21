package boomerang.scene.jimple

import boomerang.scene.Field
import boomerang.util.AccessPath
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import soot.Local
import soot.RefType
import soot.SootField
import soot.util.Chain

object AccessPathParser {
  def parseAllFromString(value: Nothing, m: Nothing): Nothing = {
    val results = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (v <- value.split(";")) {
      results.add(parseAccessPathFromString(v, m))
    }
    results
  }

  private def parseAccessPathFromString(value: Nothing, m: Nothing) = {
    var fieldNames = Lists.newArrayList
    var baseName: Nothing = null
    val overApproximated = value.endsWith("*")
    if (!value.contains("[")) baseName = value
    else {
      val i = value.indexOf("[")
      baseName = value.substring(0, i)
      fieldNames = Lists.newArrayList(value.substring(i + 1, value.length - (if (!overApproximated) 1
      else 2)).split(","))
    }
    val fields = Lists.newArrayList
    val base = getLocal(m, baseName)
    var `type` = base.getType
    import scala.collection.JavaConversions._
    for (fieldName <- fieldNames) {
      if (`type`.isInstanceOf[Nothing]) {
        val refType = `type`.asInstanceOf[Nothing]
        val fieldByName = refType.getSootClass.getFieldByName(fieldName)
        fields.add(new Nothing(fieldByName))
        `type` = fieldByName.getType
      }
    }
    new Nothing(new Nothing(base, m), if (!(overApproximated)) fields
    else Sets.newHashSet(fields))
  }

  private def getLocal(m: Nothing, baseName: Nothing): Nothing = {
    val locals = m.getDelegate.getActiveBody.getLocals
    import scala.collection.JavaConversions._
    for (l <- locals) {
      if (l.getName.equals(baseName)) return l
    }
    throw new Nothing("Could not find local with name " + baseName + " in method " + m.getDelegate)
  }
}