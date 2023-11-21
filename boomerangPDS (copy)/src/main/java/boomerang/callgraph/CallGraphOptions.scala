package boomerang.callgraph

class CallGraphOptions {
  private[callgraph] def fallbackOnPrecomputedForUnbalanced = true

  private[callgraph] def fallbackOnPrecomputedOnEmpty = false
}