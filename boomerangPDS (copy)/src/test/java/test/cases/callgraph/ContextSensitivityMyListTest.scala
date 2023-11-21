package test.cases.callgraph

import java.util
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object ContextSensitivityMyListTest {
  private[callgraph] class MyList extends Nothing {
    @Override def size: Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override def isEmpty: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def contains(o: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def iterator: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def toArray: Array[Nothing] = {
      // TODO Auto-generated method stub
      null
    }

    @Override def toArray(a: Array[Nothing]): Array[Nothing] = {
      // TODO Auto-generated method stub
      null
    }

    @Override def add(e: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def remove(o: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def containsAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def addAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def addAll(index: Int, c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def removeAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def retainAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def clear(): Unit = {

      // TODO Auto-generated method stub
    }

    @Override def get(index: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def set(index: Int, element: Nothing): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def add(index: Int, element: Nothing): Unit = {
      unreachable()
    }

    def unreachable(): Unit = {

      // TODO Auto-generated method stub
    }

    @Override def remove(index: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def indexOf(o: Nothing): Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override def lastIndexOf(o: Nothing): Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override def listIterator: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def listIterator(index: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def subList(fromIndex: Int, toIndex: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }
  }

  private[callgraph] class MyCorrectList extends Nothing {
    @Override def size: Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override def isEmpty: Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def contains(o: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def iterator: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def toArray: Array[Nothing] = {
      // TODO Auto-generated method stub
      null
    }

    @Override def toArray(a: Array[Nothing]): Array[Nothing] = {
      // TODO Auto-generated method stub
      null
    }

    @Override def add(e: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def remove(o: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def containsAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def addAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def addAll(index: Int, c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def removeAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def retainAll(c: Nothing): Boolean = {
      // TODO Auto-generated method stub
      false
    }

    @Override def clear(): Unit = {

      // TODO Auto-generated method stub
    }

    @Override def get(index: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def set(index: Int, element: Nothing): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def add(index: Int, element: Nothing): Unit = {

      // TODO Auto-generated method stub
    }

    @Override def remove(index: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def indexOf(o: Nothing): Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override def lastIndexOf(o: Nothing): Int = {
      // TODO Auto-generated method stub
      0
    }

    @Override def listIterator: Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def listIterator(index: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }

    @Override def subList(fromIndex: Int, toIndex: Int): Nothing = {
      // TODO Auto-generated method stub
      null
    }
  }
}

class ContextSensitivityMyListTest extends Nothing {
  def wrongContext(): Unit = {
    val `type` = new ContextSensitivityMyListTest.MyList
    method(`type`)
  }

  def method(`type`: Nothing): Nothing = {
    val alloc = new Nothing
    `type`.add(alloc)
    alloc
  }

  @Test def testOnlyCorrectContextInCallGraph(): Unit = {
    wrongContext()
    val `type` = new ContextSensitivityMyListTest.MyCorrectList
    val alloc = method(`type`)
    queryFor(alloc)
  }
}