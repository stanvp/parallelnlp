package menthor.classifier
package naivebayes

import scalala.tensor.dense.DenseVector
import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import collection.Map
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution

/**
 * 
 * All estimates are logged in order to be used directly in the classification
 * 
 * classes: List[C]
 * features: List[Feature]  
 * classFeatureValues: number of times class contains feature with a given name
 * classFeatureTotal: total number of features in a class
 * featureValues: number of times feature occurs in any training samples
 * featureTotal: total number of features in any traning samples
 * sampleTotal: total number of samles
 * sampleClassTotal: number of samples per class
 */
case class NaiveBayesModel[C, S <: Sample](
  classes: List[C],
  features: List[Feature],    
  classFeatureFreqDistr: ConditionalFrequencyDistribution[C,Feature], // tf_w,c and |c|
  featureFreqDistr: FrequencyDistribution[Feature], // cf_w and C
  classSamplesFreqDistr: FrequencyDistribution[C] // N_c and N
  ) {
  def encode(sample: S): Map[Feature, Double] = sample.features.filterKeys(f => features.contains(f))
}