package menthor.classifier
package featureselector

import menthor.util.ProbabilityDistribution
import menthor.util.ConditionalProbabilityDistribution
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.CollectionUtils._
import gnu.trove.procedure.TIntDoubleProcedure

/**
 * Information gain based feature selector
 *
 * @see Yiming Yang and Jan O. Pedersen. 1997. A Comparative Study on Feature Selection in Text Categorization. In Proceedings of the Fourteenth International Conference on Machine Learning (ICML '97), Douglas H. Fisher (Ed.). Morgan Kaufmann Publishers Inc., San Francisco, CA, USA, 412-420.
 */
class IGFeatureSelector[C, S <: Sample](val N: Int) extends FeatureSelector[C] {
  /**
   * Computes the information gain and selects the best features
   */
  def select(
    classes: List[C],
    features: List[Feature],
    classSamplesFreqDistr: FrequencyDistribution[C],
    classFeatureBinaryFreqDistr: ConditionalFrequencyDistribution[C, Feature],
    featureBinaryFreqDistr: FrequencyDistribution[Feature]): Iterable[(Feature, Double)] = {

    val classEntropy = -classSamplesFreqDistr.values.map { f =>
      val p = (f / classSamplesFreqDistr.total)
      p * Math.log(p)
    }.sum

    val featuresGains = features.par.map { feature =>
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
    }

    topNs(N, featuresGains.seq)(Ordering[Double].on(_._2))
  }

  def select(
    classes: List[C],
    samples: Iterable[(C, S)]): Iterable[(Feature, Double)] = {

    val featureFreqDistr = new FrequencyDistribution[Feature]
    val classSamplesFreqDistr = new FrequencyDistribution[C]

    val classFeatureBinaryFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    val featureBinaryFreqDistr = new FrequencyDistribution[Feature]

    for ((cls, sample) <- samples) {
      sample.features.forEachEntry(new TIntDoubleProcedure {
        override def execute(feature: Int, value: Double): Boolean = {
          featureFreqDistr.increment(feature, value)
          classFeatureBinaryFreqDistr(cls).increment(feature)
          featureBinaryFreqDistr.increment(feature)
          true
        }
      })

      classSamplesFreqDistr.increment(cls)
    }

    select(
      classes,
      featureFreqDistr.samples.toList,
      classSamplesFreqDistr,
      classFeatureBinaryFreqDistr,
      featureBinaryFreqDistr)
  }
}