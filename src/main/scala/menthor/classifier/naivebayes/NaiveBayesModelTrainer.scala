package menthor.classifier
package naivebayes

import menthor.classifier.FeatureSelector
import menthor.classifier.Sample
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution
import scala.util.logging.Logged
import gnu.trove.procedure.TIntDoubleProcedure

class NaiveBayesTrainer[C, S <: Sample](features: List[Feature]) extends Trainer[C, S] with Logged {
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S] = {

    log("Started NaiveBayesTrainer")

    val classFeatureFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    val featureFreqDistr = new FrequencyDistribution[Feature]

    val classSamplesFreqDistr = new FrequencyDistribution[C]
    val classFeatureBinaryFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    val featureBinaryFreqDistr = new FrequencyDistribution[Feature]

    log("Processing samples")

    for ((cls, sample) <- samples) {

      sample.features.forEachEntry(new TIntDoubleProcedure {
        override def execute(feature: Int, value: Double): Boolean = {
          classFeatureFreqDistr(cls).increment(feature, value)
          featureFreqDistr.increment(feature, value)

          classFeatureBinaryFreqDistr(cls).increment(feature)
          featureBinaryFreqDistr.increment(feature)
          true
        }
      })

      classSamplesFreqDistr.increment(cls)
    }

    val model = new NaiveBayesModel[C, S](
      classes,
      features,
      classFeatureFreqDistr,
      featureFreqDistr,
      classSamplesFreqDistr)

    val classifier = new NaiveBayesClassifier[C, S](model)

    log("Finished NaiveBayesTrainer")

    classifier
  }
}