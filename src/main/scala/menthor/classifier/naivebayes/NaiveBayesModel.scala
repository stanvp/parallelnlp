package menthor.classifier
package naivebayes

import scalala.tensor.dense.DenseVector
import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import collection.Map
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution
import scala.collection.immutable.HashSet
import gnu.trove.map.hash.TIntDoubleHashMap
import gnu.trove.procedure.TIntDoubleProcedure

/**
 *
 * All estimates are logged in order to be used directly in the classification
 *
 */
case class NaiveBayesModel[C, S <: Sample](
  classes: List[C],
  features: List[Feature],
  classFeatureFreqDistr: ConditionalFrequencyDistribution[C, Feature], // tf_w,c and |c|
  featureFreqDistr: FrequencyDistribution[Feature], // cf_w and C
  classSamplesFreqDistr: FrequencyDistribution[C] // N_c and N
  ) {
  val featureset = HashSet(features: _*)
  def encode(sample: S): TIntDoubleHashMap = {
    val result = new TIntDoubleHashMap

    sample.features.forEachEntry(new TIntDoubleProcedure {
      override def execute(feature: Int, value: Double): Boolean = {
        if (featureset.contains(feature)) {
          result.put(feature, value)
        }
        true
      }
    })
    
    result
  }
}