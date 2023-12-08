package boomerang.flowfunction

import boomerang.scene.DeclaredMethod

object FlowFunctionUtils {
  def isSystemArrayCopy(method: DeclaredMethod): Boolean = {
    method.getName.equals("arraycopy") && method.getDeclaringClass.getName.equals("java.lang.System")
  }
}