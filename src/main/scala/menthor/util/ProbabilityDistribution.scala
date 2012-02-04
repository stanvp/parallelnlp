package menthor.util

import scalala.library.Numerics._
import scalala.operators.Implicits._
import scala.collection.mutable.HashMap

/**
 * Represents log probability distribution.
 * Inspired by NLTK <http://www.nltk.org/>
 * 
 * @author Stanislav Peshterliev
 */
class ProbabilityDistribution[T](_distribution: Map[T, Double], normalize: Boolean = false) {
  val distribution = if (normalize) ProbabilityDistribution.normalizeLogProbabilty(_distribution) else _distribution 

  def prob(key: T) = distribution.get(key).map(Math.exp(_))
}

object ProbabilityDistribution {
  implicit def delegateToMap[T](probabilityDistribution: ProbabilityDistribution[T]): Map[T, Double] = probabilityDistribution.distribution
  
  /**
   * Normalize logarithmic probability distribution such that it sums to 1
   * 
   * @param dist probability distribution map
   * @return normalized probability distribution
   */
  def normalizeLogProbabilty[T](dist: Map[T, Double]): Map[T, Double] = {
    val sum = logSum(dist.values.toSeq)

    if (sum <= Double.NegativeInfinity) {
      val logp = Math.log(1.0 / dist.size)
      dist.mapValues(p => logp)
    } else {
      dist.mapValues(p => p - sum)
    }
  }  
}

/**
 * Represents log probability distribution given some condition.
 * Inspired by NLTK <http://www.nltk.org/>
 * 
 * @author Stanislav Peshterliev
 */
class ConditionalProbabilityDistribution[U,T](val distributions: Map[U,ProbabilityDistribution[T]]) { }

object ConditionalProbabilityDistribution {
  implicit def delegateToMap[U,T](conditionalProbabilityDistribution: ConditionalProbabilityDistribution[U,T]): Map[U,ProbabilityDistribution[T]] = conditionalProbabilityDistribution.distributions
}