package menthor.util

import scalala.library.Numerics._
import scala.collection.mutable.HashMap
import gnu.trove.map.hash.TObjectDoubleHashMap

class FrequencyDistribution[T] {
  private[this] val distribution = new TObjectDoubleHashMap[T]()
  private var _total = 0.0
  
  def increment(sample: T, value: Double = 1.0) {
	  distribution.put(sample, distribution.get(sample) + value)
	  _total += value
  }
  
  def total = _total
  
  def samples = distribution.keys.map(_.asInstanceOf[T]).toList
  
  def count(sample: T) : Double = distribution.get(sample)
  
  def frequency(sample: T) = if (total == 0) 0 else distribution.get(sample) / total 
  
  def toMap : TObjectDoubleHashMap[T] = distribution
}

object FrequencyDistribution {
  implicit def convertToMap[T](frequencyDistribution: FrequencyDistribution[T]): TObjectDoubleHashMap[T] = frequencyDistribution.toMap
}

class ConditionalFrequencyDistribution[U,T] {
  private[this] val distributions = new HashMap[U,FrequencyDistribution[T]]
  
  def toMap : Map[U, TObjectDoubleHashMap[T]] = distributions.mapValues(_.toMap).toMap
  
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
  implicit def convertToMap[U,T](conditionalFrequencyDistribution: ConditionalFrequencyDistribution[U,T]): Map[U,TObjectDoubleHashMap[T]] = conditionalFrequencyDistribution.toMap
}