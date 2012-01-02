package menthor.classifier
package maxent

import scala.collection.mutable.HashMap
import menthor.processing.Graph
import menthor.processing.Message
import menthor.processing.Substep
import menthor.processing.Vertex
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Numerics._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution
import scala.util.logging.Logged

class MaxentTrainerParallel[C, S <: Sample](partitions: Int, featureSelector: FeatureSelector[C]) extends Trainer[C, S] with Logged {
  val iterations = 20

  /**
   * Train maximum entropy classifier
   */
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S] = {

    log("Started MaxentTrainerParallel")

    log("Selecting features")

    val featureshash = selectFeatures(classes, samples)

    val model = new MaxentModelCached[C, S](new MaxentModel[C, S](
      classes,
      featureshash,
      DenseVector.zeros[Double](featureshash.size * classes.size)))

    val classifier = new MaxentClassifier[C, S](model)

    log("Building the graph")

    val graph = new Graph[ProcessingResult]

    val masters = (for (i <- 0 to partitions - 1) yield new MasterVertex[C, S]("master" + i, classifier, i, null)).toList

    for (master <- masters) {
      master.masters = masters.filterNot(_.label == master.label)
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

    log("Processing samples")

    graph.start()
    graph.iterate(iterations * 7)

    graph.terminate()

    log("Finished MaxentTrainerParallel")

    val cachedModel = graph.vertices.first.asInstanceOf[SampleVertex[C, S]].classifier.model

    new MaxentClassifier[C, S](new MaxentModel[C, S](cachedModel.classes, cachedModel.featureshash, cachedModel.parameters))
  }

  def selectFeatures(
    classes: List[C],
    samples: Iterable[(C, S)]): Map[Int, List[Feature]] = {

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

    val featureshash = new HashMap[Int, List[Feature]].withDefaultValue(List[Feature]())

    for ((f, ig) <- features) {
      val key = Math.abs(f.hashCode()) % 200
      featureshash.put(key, f :: featureshash(key))
    }

    featureshash.toMap
  }

  class MasterVertex[C, S <: Sample](label: String, val classifier: MaxentClassifier[C, S], group: Int, var masters: List[MasterVertex[C, S]])
    extends Vertex[ProcessingResult](label, new ProcessingResult) {

    var iteration = 0

    val model = classifier.model

    var logEmpiricalFeatureFreqDistr: Vector[Double] = _
    var logEstimatedFeatureFreqDistr: Vector[Double] = DenseVector.zeros[Double](model.parameters.size)

    def update(): Substep[ProcessingResult] = {
      {
        // superstep == 0 - sample step
        List()
      } then {
        if (superstep == 1) {
          val empiricalFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

          incoming.map(_.value.empiricalFeatureFreqDistr).foreach { x =>
            x.foreachNonZeroPair { (i, v) =>
              empiricalFeatureFreqDistr(i) += v
            }
          }

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
        logEstimatedFeatureFreqDistr(0 to logEstimatedFeatureFreqDistr.size - 1) := 0.0
        
        incoming.map(_.value.logEstimatedFeatureFreqDistr).foreach { x => 
          x.foreachPair { (i, v) =>
            logEstimatedFeatureFreqDistr(i) = logSum(logEstimatedFeatureFreqDistr(i), v)
          }
        }
        
        classifier.model.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)

        value.parameters = classifier.model.parameters

        for (master <- masters) yield Message(this, master, this.value)
      } then {
        iteration += 1
        log(label + ": Iteration " + iteration)

        value.parameters = incoming.map(_.value.parameters).reduce { (x, y) => x + y } / masters.size

        classifier.model.parameters(0 to classifier.model.parameters.size - 1) := value.parameters

        //log("Parameters: " + value.parameters)

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
        val logEstimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

        val dist = classifier.probClassify(sample)

        for ((distcls, prob) <- dist) {
          model.encode(distcls, sample).foreachNonZeroPair { (i, v) =>
            logEstimatedFeatureFreqDistr(i) = logSum(logEstimatedFeatureFreqDistr(i), Math.log(v) + prob)
          }          
        }

        value.logEstimatedFeatureFreqDistr = logEstimatedFeatureFreqDistr

        for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
      } then {
        // master step
        List()
      } then {
        // master step
        List()
      } then {
        classifier.model.parameters(0 to classifier.model.parameters.size - 1) := incoming.first.value.parameters
        List()
      }
    }
  }

  case class ProcessingResult {
    var empiricalFeatureFreqDistr: Vector[Double] = _
    var logEstimatedFeatureFreqDistr: Vector[Double] = _
    var logEmpiricalFeatureFreqDistr: Vector[Double] = _
    var parameters: Vector[Double] = _
  }
}