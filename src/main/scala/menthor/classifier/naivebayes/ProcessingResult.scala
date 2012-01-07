package menthor.classifier
package naivebayes

import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution

class ProcessingResult[C] {
  var classFeatureFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
  var featureFreqDistr = new FrequencyDistribution[Feature]
  var classSamplesFreqDistr = new FrequencyDistribution[C]
}

object ProcessingResult {
  def merge[C](v: ProcessingResult[C], results: List[ProcessingResult[C]]) {
    v.classFeatureFreqDistr = ConditionalFrequencyDistribution.merge(results.map(_.classFeatureFreqDistr))
    v.featureFreqDistr = FrequencyDistribution.merge(results.map(_.featureFreqDistr))
    v.classSamplesFreqDistr = FrequencyDistribution.merge(results.map(_.classSamplesFreqDistr))
  }
}