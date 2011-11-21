package menthor.util

object MathUtils {
  def log2(n: Double) = {
    Math.log(n) / Math.log(2)
  }

  /**
   *  Given two numbers C{logx}=M{log(x)} and C{logy}=M{log(y)}, return
   *  M{log(x+y)}.  Conceptually, this is the same as returning
   *  M{log(2**(C{logx})+2**(C{logy}))}, but the actual implementation
   *  avoids overflow errors that could result from direct computation.
   *
   *  @see http://nltk.github.com/api/nltk.html Probability Module
   */
  def addLogs(logx: Double, logy: Double) = {
    val _ADD_LOGS_MAX_DIFF = log2(1e-30)

    if (logx < logy + _ADD_LOGS_MAX_DIFF) {
      logy
    } else if (logy < logx + _ADD_LOGS_MAX_DIFF) {
      logx
    } else {
      val base = Math.min(logx, logy)
      base + log2(Math.pow((logx - base), 2) + Math.pow((logy - base), 2))
    }
  }

  /**
   * Normalize logarithmic probability distribution such that it sums to 1
   */
  def normalizeLogProbabilty[C](dist: Map[C, Double]): Map[C, Double] = {
    val sum = dist.values.sum

    if (sum <= Double.NegativeInfinity) {
      val logp = 1.0 / dist.size
      dist.mapValues(p => logp)
    } else {
      dist.mapValues(p => Math.pow((p - sum),2))
    }
  }
}