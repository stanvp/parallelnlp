package menthor.classifier
package maxent

import scala.collection.mutable.HashMap
import menthor.processing.Graph
import menthor.processing.Message
import menthor.processing.Substep
import menthor.processing.Vertex
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution

object MaxentTrainerParallel {
  val iterations = 100

  /**
   * Train maximum entropy classifier
   */
  def train[C, S <: Sample](
    classes: List[C],
    samples: List[(C, S)],
    partitions: Int,
    featureSelector: FeatureSelector[C] = new FeatureSelector[C]): MaxentClassifier[C, S] = {

    val features = selectFeatures(classes, samples, featureSelector)

    val model = new MaxentModel[C, S](
      classes,
      features,
      DenseVector.zeros[Double](features.size))

    val classifier = new MaxentClassifier[C, S](model)

    val graph = new Graph[ProcessingResult]

    val masters = (for (i <- 0 to partitions - 1) yield new MasterVertex[C, S]("master" + i, classes.first, samples.first._2, classifier, i, null)).toList

    for (master <- masters) {
      master.masters = masters
      graph.addVertex(master)
    }

    for ((group, i) <- samples.grouped(Math.ceil(samples.size.toDouble / partitions).toInt).zipWithIndex) {
      for ((c, sample) <- group) {
        val vertex = new SampleVertex(sample.toString, c, sample, classifier, i, masters)
        graph.addVertex(vertex)
        vertex.connectTo(masters(i))
        masters(i).connectTo(vertex)
      }
    }

    graph.start()
    graph.iterate(iterations * 7)

    graph.terminate()

    graph.vertices.first.asInstanceOf[SampleVertex[C, S]].classifier
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

class MasterVertex[C, S <: Sample](label: String, cls: C, sample: S, val classifier: MaxentClassifier[C, S], group: Int, var masters: List[MasterVertex[C, S]])
  extends Vertex[ProcessingResult](label, new ProcessingResult) {
  
  var iteration = 0

  val model = classifier.model

  var logEmpiricalFeatureFreqDistr: Vector[Double] = _
  var logEstimatedFeatureFreqDistr: Vector[Double] = _

  def update(): Substep[ProcessingResult] = {
    {
      // superstep == 0 - sample step
      List()
    } then {
      if (superstep == 1) {
        val empiricalFeatureFreqDistr = incoming.map(_.value.empiricalFeatureFreqDistr).reduce { (x, y) => x + y }

        logEmpiricalFeatureFreqDistr = empiricalFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))
        value.logEmpiricalFeatureFreqDistr = logEmpiricalFeatureFreqDistr

        for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
      } else {
        List()
      }
    } then {
      // superstep == 2 sample step
      List()
    } then {
      // sample step
      List()
    } then {
      val estimatedFeatureFreqDistr = incoming.map(_.value.estimatedFeatureFreqDistr).reduce { (x, y) => x + y }

      logEstimatedFeatureFreqDistr = estimatedFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))

      classifier.model.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)

      //println("Parameters: " + classifier.model.parameters)

      value.parameters = classifier.model.parameters

      for (master <- masters) yield Message(this, master, this.value)
    } then {
      iteration += 1
      println(label + ": Iteration " + iteration)
      value.parameters = incoming.map(_.value.parameters).reduce { (x, y) => x + y } / masters.size

      for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
    } then {
      // sample step
      List()
    }
  }
}

class SampleVertex[C, S <: Sample](label: String, cls: C, sample: S, val classifier: MaxentClassifier[C, S], group: Int, var masters: List[MasterVertex[C, S]])
  extends Vertex[ProcessingResult](label, new ProcessingResult) {

  val model = classifier.model

  var logEmpiricalFeatureFreqDistr: Vector[Double] = _
  var logEstimatedFeatureFreqDistr: Vector[Double] = _

  def update(): Substep[ProcessingResult] = {
    {
      if (superstep == 0) {
        value.empiricalFeatureFreqDistr = model.encode(cls, sample)

        for (neighbor <- masters) yield Message(this, neighbor, this.value)
      } else {
        List()
      }
    } then {
      // superstep == 1 - masters step
      List()
    } then {
      if (superstep == 2) {
        logEmpiricalFeatureFreqDistr = incoming.first.value.logEmpiricalFeatureFreqDistr
        value.logEmpiricalFeatureFreqDistr = logEmpiricalFeatureFreqDistr
      }
      List()
    } then {
      val estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.features.size)

      val dist = classifier.probClassify(sample)

      for ((distcls, prob) <- dist) {
        estimatedFeatureFreqDistr += (model.encode(distcls, sample) * Math.exp(prob))
      }

      value.estimatedFeatureFreqDistr = estimatedFeatureFreqDistr

      for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
    } then {
      // master step
      List()
    } then {
      // master step
      List()
    } then {
      val parameters = incoming.first.value.parameters

      classifier.model.parameters(0 to classifier.model.parameters.size - 1) := parameters
      List()
    }
  }
}

case class ProcessingResult {
  var empiricalFeatureFreqDistr: Vector[Double] = _
  var estimatedFeatureFreqDistr: Vector[Double] = _
  var logEmpiricalFeatureFreqDistr: Vector[Double] = _
  var parameters: Vector[Double] = _
}