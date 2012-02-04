package menthor.classifier
package naivebayes

import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution

/**
 * The data value type for Menthor vertices 
 * 
 * @author Stanislav Peshterliev
 */
class ProcessingResult[C] {
  var classFeatureFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
  var featureFreqDistr = new FrequencyDistribution[Feature]
  var classSamplesFreqDistr = new FrequencyDistribution[C]
}

/**
 * Utilities for working with processing results
 * 
 * @author Stanislav Peshterliev
 */
object ProcessingResult {
  
  /**
   * Merges a list of processing results
   * 
   * @param v resulting processing result
   * @param results list of processing results 
   */
  def merge[C](v: ProcessingResult[C], results: List[ProcessingResult[C]]) {
    v.classFeatureFreqDistr = ConditionalFrequencyDistribution.merge(results.map(_.classFeatureFreqDistr))
    v.featureFreqDistr = FrequencyDistribution.merge(results.map(_.featureFreqDistr))
    v.classSamplesFreqDistr = FrequencyDistribution.merge(results.map(_.classSamplesFreqDistr))
  }
}