package boomerang.scene.jimple

import boomerang.scene.{Field, JimpleField, JimpleMethod, JimpleVal}
import boomerang.util.AccessPath
import com.google.common.collect.{Lists, Sets}
import soot.{Local, RefType, SootField}
import soot.util.Chain

import scala.jdk.CollectionConverters._

object AccessPathParser {

  def parseAllFromString(value: String, m: JimpleMethod): Collection[_ <: AccessPath] = {
    val results = Sets.newHashSet[AccessPath]()
    for (v <- value.split(";")) {
      results.add(parseAccessPathFromString(v, m))
    }
    results
  }

  private def parseAccessPathFromString(value: String, m: JimpleMethod): AccessPath = {
    val fieldNames = Lists.newArrayList[String]()
    val baseName: String
    val overApproximated = value.endsWith("*")
    if (!value.contains("[")) {
      baseName = value
    } else {
      val i = value.indexOf("[")
      baseName = value.substring(0, i)
      fieldNames.addAll(
        Lists.newArrayList(
          value.substring(i + 1, value.length - (if (!overApproximated) 1 else 2)).split(","): _*
        )
      )
    }
    val fields = Lists.newArrayList[Field]()
    val base = getLocal(m, baseName)
    var `type` = base.getType
    for (fieldName <- fieldNames.asScala) {
      if (`type`.isInstanceOf[RefType]) {
        val refType = `type`.asInstanceOf[RefType]
        val fieldByName = refType.getSootClass.getFieldByName(fieldName)
        fields.add(new JimpleField(fieldByName))
        `type` = fieldByName.getType
      }
    }
    new AccessPath(
      new JimpleVal(base, m),
      if (!overApproximated) fields else Sets.newHashSet(fields.asScala.toSeq: _*)
    )
  }

  private def getLocal(m: JimpleMethod, baseName: String): Local = {
    val locals = m.getDelegate.getActiveBody.getLocals
    for (l <- locals.asScala) {
      if (l.getName.equals(baseName)) return l
    }
    throw new RuntimeException(
      "Could not find local with name " + baseName + " in method " + m.getDelegate
    )
  }
}