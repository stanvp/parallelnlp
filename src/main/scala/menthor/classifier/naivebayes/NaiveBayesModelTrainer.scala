package menthor.classifier
package naivebayes

import menthor.classifier.FeatureSelector
import menthor.classifier.Sample
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution

object NaiveBayesTrainer {
  def train[C, S <: Sample](
      classes: List[C], 
      samples: List[(C, S)], 
      featureSelector : FeatureSelector[C] = new FeatureSelector[C]): NaiveBayesClassifier[C, S] = {
    
    val classFeatureFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    val featureFreqDistr = new FrequencyDistribution[Feature]
    
    val classSamplesFreqDistr = new FrequencyDistribution[C]
    val classFeatureBinaryFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    val featureBinaryFreqDistr = new FrequencyDistribution[Feature]

    for ((cls, sample) <- samples) {
      for ((feature, value) <- sample.features) {
        classFeatureFreqDistr(cls).increment(feature, value)
        featureFreqDistr.increment(feature, value)
        
        classFeatureBinaryFreqDistr(cls).increment(feature)
        featureBinaryFreqDistr.increment(feature)        
      }

      classSamplesFreqDistr.increment(cls)
    }
    
    val features = featureSelector.select(
      classes,
      featureFreqDistr.samples,
      classSamplesFreqDistr,
      classFeatureBinaryFreqDistr,
      featureBinaryFreqDistr
    ).slice(0,100)
    
    val model = new NaiveBayesModel[C, S](
      classes,
      features.map(_._1).toList,
      classFeatureFreqDistr,
      featureFreqDistr,
      classSamplesFreqDistr)

    val classifier = new NaiveBayesClassifier[C, S](model)

    classifier
  }
}