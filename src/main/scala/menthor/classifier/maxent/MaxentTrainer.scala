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
import scalala.library.Numerics._
import java.lang.Math
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution
import scala.util.logging.Logged
import scalala.tensor.sparse.SparseVector

class MaxentTrainer[C, S <: Sample](featureSelector: FeatureSelector[C]) extends Trainer[C, S] with Logged {
  val iterations = 20

  /**
   * Train maximum entropy classifier
   */
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S] = {

    log("Started MaxentTrainer")

    log("Selecting features")

    val features = selectFeatures(classes, samples)

    val model = new MaxentModelCached[C, S](new MaxentModel[C, S](
      classes,
      features,
      DenseVector.ones[Double](features.size * classes.size)))

    val classifier = new MaxentClassifier[C, S](model)

    log("Processing samples")

    val logEmpiricalFeatureFreqDistr = calculateFeatureFrequencyDistribution(classes, samples, model).map(x => if (x == 0.0) 0.0 else Math.log(x))

    val logEstimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

    for (n <- 1 to iterations) {
      log("Iteration: " + n)

      logEstimatedFeatureFreqDistr(0 to logEstimatedFeatureFreqDistr.size - 1) := 0.0

      for ((_, sample) <- samples) {
        val dist = classifier.probClassify(sample)
        for ((distcls, prob) <- dist) {
          model.encode(distcls, sample).foreachNonZeroPair { (i, v) =>
            logEstimatedFeatureFreqDistr(i) = logSum(logEstimatedFeatureFreqDistr(i), Math.log(v) + prob)
          }
        }
      }

      classifier.model.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)

      //println("Parameters: " + classifier.model.parameters)
    }

    log("Finished MaxentTrainer")

    new MaxentClassifier[C, S](new MaxentModel[C, S](model.classes, model.features, model.parameters))
  }

  def calculateFeatureFrequencyDistribution[C, S <: Sample](
    classes: List[C],
    samples: Iterable[(C, S)],
    model: MaxentModel[C, S]): Vector[Double] = {

    val featureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

    for ((cls, sample) <- samples) {
      model.encode(cls, sample).foreachNonZeroPair { (i, v) =>
        featureFreqDistr(i) += v
      }
    }

    featureFreqDistr
  }

  def selectFeatures(
    classes: List[C],
    samples: Iterable[(C, S)]) : List[Feature] = {

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
      featureBinaryFreqDistr)

     features.map(_._1).toList
  }
}