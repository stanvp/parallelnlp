package menthor.classifier

import menthor.util.ProbabilityDistribution
import menthor.util.ConditionalProbabilityDistribution
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution

/**
 * Generic feature selector interface with a single method select
 * 
 * @author Stanislav Peshterliev
 */
trait FeatureSelector[C] {
  val N : Int
  
  def select(
    classes: List[C],
    features: List[Feature],
    classSamplesFreqDistr: FrequencyDistribution[C],
    classFeatureBinaryFreqDistr: ConditionalFrequencyDistribution[C, Feature],
    featureBinaryFreqDistr: FrequencyDistribution[Feature]): Iterable[(Feature, Double)]
}