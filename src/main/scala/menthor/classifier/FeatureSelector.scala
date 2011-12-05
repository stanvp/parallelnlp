package menthor.classifier

import menthor.util.ProbabilityDistribution
import menthor.util.ConditionalProbabilityDistribution
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution

/**
 * Information gain based feature selector
 *
 * @see Yiming Yang and Jan O. Pedersen. 1997. A Comparative Study on Feature Selection in Text Categorization. In Proceedings of the Fourteenth International Conference on Machine Learning (ICML '97), Douglas H. Fisher (Ed.). Morgan Kaufmann Publishers Inc., San Francisco, CA, USA, 412-420.
 */
class FeatureSelector[C] {

  /**
   * Computes the information gain and selects the best features
   */
  def select(
    classes: List[C],
    features: List[Feature],
    classSamplesFreqDistr: FrequencyDistribution[C],
    classFeatureBinaryFreqDistr: ConditionalFrequencyDistribution[C, Feature],
    featureBinaryFreqDistr: FrequencyDistribution[Feature]): Iterable[(String, Double)] = {
    
    val classEntropy = -classSamplesFreqDistr.values.map { f =>
      val p = (f / classSamplesFreqDistr.total)
      p * Math.log(p)
    }.sum

    features.map { feature =>
      var classFeatureEntropy = 0.0
      
      for (w <- List(1, 0)) {
        val featureProb = Math.abs(w - (featureBinaryFreqDistr.count(feature) / classSamplesFreqDistr.total))
     
        for (c <- classes) {
          val totalFeatureCount = Math.abs((w * classSamplesFreqDistr.total) - featureBinaryFreqDistr.count(feature))
          val featureCount = Math.abs((w * classSamplesFreqDistr.count(c)) - classFeatureBinaryFreqDistr(c).count(feature))          

          val classFeatureProb = if (totalFeatureCount == 0) 0 else featureCount / totalFeatureCount

          classFeatureEntropy += featureProb * classFeatureProb * (if (classFeatureProb == 0) 0.0 else Math.log(classFeatureProb) / Math.log(2))
        }
      }
      
      val ig = classEntropy + classFeatureEntropy
      (feature, ig)
    }.sort((e1, e2) => (e1._2 compareTo e2._2) > 0)
  }
}