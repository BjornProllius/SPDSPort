package boomerang.scene

import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import wpds.interfaces.Location

object Method {
  private var epsilon: Method = null

  def epsilon: Method = {
    if (epsilon == null) epsilon = new Method() {
      @Override def hashCode: Int = System.identityHashCode(this)

      @Override def equals(obj: Nothing): Boolean = obj eq this

      @Override override def isStaticInitializer: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isParameterLocal(`val`: Nothing): Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isThisLocal(`val`: Nothing): Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getLocals: Nothing = {
        // TODO Auto-generated method stub
        Sets.newHashSet
      }

      @Override override def getThisLocal: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def getParameterLocals: Nothing = {
        // TODO Auto-generated method stub
        Lists.newArrayList
      }

      @Override override def isStatic: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isNative: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def getStatements: Nothing = {
        // TODO Auto-generated method stub
        Lists.newArrayList
      }

      @Override override def getDeclaringClass: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def getControlFlowGraph: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def getSubSignature: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def getName: Nothing = {
        // TODO Auto-generated method stub
        null
      }

      @Override override def isConstructor: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override override def isPublic: Boolean = {
        // TODO Auto-generated method stub
        false
      }
    }
    epsilon
  }
}

abstract class Method protected extends Nothing {
  @Override def toString = "METHOD EPS"

  def isStaticInitializer: Boolean

  def isParameterLocal(`val`: Nothing): Boolean

  def isThisLocal(`val`: Nothing): Boolean

  def getLocals: Nothing

  def getThisLocal: Nothing

  def getParameterLocals: Nothing

  def isStatic: Boolean

  def isNative: Boolean

  def getStatements: Nothing

  def getDeclaringClass: Nothing

  def getControlFlowGraph: Nothing

  def getSubSignature: Nothing

  def getName: Nothing

  def getParameterLocal(i: Int): Nothing = getParameterLocals.get(i)

  def isConstructor: Boolean

  def isPublic: Boolean

  private var returnLocals: Nothing = null

  def getReturnLocals: Nothing = {
    if (returnLocals == null) {
      returnLocals = Sets.newHashSet
      import scala.collection.JavaConversions._
      for (s <- getStatements) {
        if (s.isReturnStmt) returnLocals.add(s.getReturnOp)
      }
    }
    returnLocals
  }

  @Override def accepts(other: Nothing): Boolean = this.equals(other)
}