package menthor.util

import scalala.library.Numerics._
import scala.collection.mutable.HashMap

class FrequencyDistribution[T] {
  private[this] val distribution = new HashMap[T,Double].withDefaultValue(0)
  private var _total = 0.0
  
  def increment(sample: T, value: Double = 1.0) {
	  distribution.put(sample, distribution(sample) + value)
	  _total += value
  }
  
  def total = _total
  
  def samples = distribution.keys.toList
  
  def count(sample: T) : Double = distribution.getOrElse(sample, 0.0)
  
  def frequency(sample: T) = if (total == 0) 0 else distribution.getOrElse(sample, 0.0) / total
  
  def toMap : Map[T, Double] = distribution.toMap
}

object FrequencyDistribution {
  implicit def convertToMap[T](frequencyDistribution: FrequencyDistribution[T]): Map[T,Double] = frequencyDistribution.toMap
}

class ConditionalFrequencyDistribution[U,T] {
  private[this] val distributions = new HashMap[U,FrequencyDistribution[T]]
  
  def toMap : Map[U, Map[T, Double]] = distributions.mapValues(_.toMap).toMap
  
  def samples = distributions.keys.toList
  
  def apply(condition: U) = {
    distributions.get(condition) match {
      case Some(distribution) => distribution
      case None => {
        val distribution = new FrequencyDistribution[T]
        distributions.put(condition, distribution)
        distribution
      }
    }
  }
}

object ConditionalFrequencyDistribution {
  implicit def convertToMap[U,T](conditionalFrequencyDistribution: ConditionalFrequencyDistribution[U,T]): Map[U,Map[T, Double]] = conditionalFrequencyDistribution.toMap
}