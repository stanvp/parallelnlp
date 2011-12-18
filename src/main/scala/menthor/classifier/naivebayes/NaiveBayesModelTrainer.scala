package menthor.classifier
package naivebayes

import menthor.classifier.FeatureSelector
import menthor.classifier.Sample
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution
import scala.util.logging.Logged

class NaiveBayesTrainer[C, S <: Sample](featureSelector: FeatureSelector[C] = new FeatureSelector[C]) extends Trainer[C, S] with Logged {
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
      for ((feature, value) <- sample.features) {
        classFeatureFreqDistr(cls).increment(feature, value)
        featureFreqDistr.increment(feature, value)

        classFeatureBinaryFreqDistr(cls).increment(feature)
        featureBinaryFreqDistr.increment(feature)
      }

      classSamplesFreqDistr.increment(cls)
    }

    log("Selecting features")
    
    val features = featureSelector.select(
      100,
      classes,
      featureFreqDistr.samples,
      classSamplesFreqDistr,
      classFeatureBinaryFreqDistr,
      featureBinaryFreqDistr)

    val model = new NaiveBayesModel[C, S](
      classes,
      features.map(_._1).toList,
      classFeatureFreqDistr,
      featureFreqDistr,
      classSamplesFreqDistr)

    val classifier = new NaiveBayesClassifier[C, S](model)
    
    log("Finished NaiveBayesTrainer")

    classifier
  }
}