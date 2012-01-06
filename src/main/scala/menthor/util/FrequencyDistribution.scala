package menthor.util

import scalala.library.Numerics._
import scala.collection.mutable.HashMap
import gnu.trove.map.hash.TObjectDoubleHashMap
import scala.collection.JavaConversions._

class FrequencyDistribution[T](_distribution : TObjectDoubleHashMap[T] = new TObjectDoubleHashMap[T]()) {
  private[this] val distribution : TObjectDoubleHashMap[T] = _distribution
  private var _total = 0.0
  
  def increment(sample: T, value: Double = 1.0) {
	  distribution.put(sample, distribution.get(sample) + value)
	  _total += value
  }
  
  def total = _total
  
  def samples = distribution.keys.map(_.asInstanceOf[T])
  
  def count(sample: T) : Double = distribution.get(sample)
  
  def frequency(sample: T) = if (total == 0) 0 else distribution.get(sample) / total 
  
  def toMap : TObjectDoubleHashMap[T] = distribution
}

object FrequencyDistribution {
  implicit def convertToMap[T](frequencyDistribution: FrequencyDistribution[T]): TObjectDoubleHashMap[T] = frequencyDistribution.toMap
  
  def merge[T](distributions : List[FrequencyDistribution[T]]) : FrequencyDistribution[T] = {
    new FrequencyDistribution[T](TMerger.merge(distributions.map(_.toMap)))
  }
}

class ConditionalFrequencyDistribution[U,T](_distributions : HashMap[U,FrequencyDistribution[T]] = new HashMap[U,FrequencyDistribution[T]]) {
  private[this] val distributions = _distributions
  
  def toMap : Map[U, FrequencyDistribution[T]] = distributions.toMap
  
  def samples = distributions.keys
  
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
  implicit def convertToMap[U,T](conditionalFrequencyDistribution: ConditionalFrequencyDistribution[U,T]): Map[U,FrequencyDistribution[T]] = conditionalFrequencyDistribution.toMap
  
  def merge[U,T](distributions : List[ConditionalFrequencyDistribution[U,T]]) : ConditionalFrequencyDistribution[U,T] = {
    val result = new HashMap[U, List[FrequencyDistribution[T]]]
    
    for (distribution <- distributions) {
       for ((key, value) <- distribution) {
         result.put(key, value :: result.getOrElse(key, Nil))
       }
    }
    
    val t = result.flatMap { case (key, values) => HashMap(key -> FrequencyDistribution.merge[T](values)) }
    
    new ConditionalFrequencyDistribution[U,T](t)
  }    
}