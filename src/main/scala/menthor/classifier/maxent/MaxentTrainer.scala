package menthor.classifier
package maxent

import scala.collection.mutable.HashMap
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution

object MaxentTrainer {
  val iterations = 100

  /**
   * Train maximum entropy classifier
   */
  def train[C, S <: Sample](
    classes: List[C],
    samples: List[(C, S)],
    featureSelector: FeatureSelector[C] = new FeatureSelector[C]): MaxentClassifier[C, S] = {

    val features = selectFeatures(classes, samples, featureSelector)

    val model = new MaxentModel[C, S](
      classes,
      features,
      DenseVector.zeros[Double](features.size))

    val classifier = new MaxentClassifier[C, S](model)

    val logEmpiricalFeatureFreqDistr = calculateFeatureFrequencyDistribution(classes, samples, model).map(Math.log)
    
    val estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.features.size)

    for (n <- 1 to iterations) {
      println("Iteration: " + n)
      
      estimatedFeatureFreqDistr(0 to estimatedFeatureFreqDistr.size - 1) := 0.0

      for ((_, sample) <- samples) {
        val dist = classifier.probClassify(sample)
        for ((distcls, prob) <- dist) {
          estimatedFeatureFreqDistr += (model.encode(distcls, sample) * Math.exp(prob))
        }
      }
      
      val logEstimatedFeatureFreqDistr = estimatedFeatureFreqDistr.map(Math.log)

      classifier.model.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)

      println("Weights: " + classifier.model.parameters)
    }

    classifier
  }

  def calculateFeatureFrequencyDistribution[C, S <: Sample](
    classes: List[C],
    samples: List[(C, S)],
    model: MaxentModel[C, S]): Vector[Double] = {

    val featureFreqDistr = DenseVector.zeros[Double](model.features.size)

    for ((cls, sample) <- samples) {
      featureFreqDistr += model.encode(cls, sample)
    }

    featureFreqDistr
  }

  def selectFeatures[C, S <: Sample](
    classes: List[C],
    samples: List[(C, S)],
    featureSelector: FeatureSelector[C] = new FeatureSelector[C]): List[MaxentFeatureFunction[C, S]] = {

    val featureFreqDistr = new FrequencyDistribution[Feature]
    val classSamplesFreqDistr = new FrequencyDistribution[C]

    val classFeatureBinaryFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    val featureBinaryFreqDistr = new FrequencyDistribution[Feature]

    for ((cls, sample) <- samples) {
      for ((feature, value) <- sample.features) {
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
      featureBinaryFreqDistr).slice(0, 100)

    for (
      c <- classes;
      (f, ig) <- features
    ) yield new MaxentFeatureFunction[C, S](f, c)
  }
}