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
import scalala.tensor.sparse.SparseVector
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import menthor.util.FrequencyDistribution
import menthor.util.ConditionalFrequencyDistribution
import scala.util.logging.Logged
import gnu.trove.procedure.TIntDoubleProcedure

class MaxentTrainerParallel[C, S <: Sample](partitions: Int, features: List[Feature], iterations: Int = 100) extends Trainer[C, S] with Logged {
  /**
   * Train maximum entropy classifier
   */
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S] = {

    log("Started MaxentTrainerParallel")

    val model = new MaxentModel[C, S](
      classes,
      features,
      DenseVector.zeros[Double](features.size * classes.size))

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

    classifier
  }

  class MasterVertex[C, S <: Sample](label: String, val classifier: MaxentClassifier[C, S], group: Int, var masters: List[MasterVertex[C, S]])
    extends Vertex[ProcessingResult](label, new ProcessingResult) {

    var iteration = 0

    val model = classifier.model

    var logEmpiricalFeatureFreqDistr: DenseVector[Double] = _

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
        val estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

        incoming.map(_.value.estimatedFeatureFreqDistr).foreach { x =>
          x.foreachPair { (i, v) =>
            estimatedFeatureFreqDistr(i) += v
          }
        }

        val logEstimatedFeatureFreqDistr = estimatedFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))

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
    val encoding = model.encode(sample)

    var logEmpiricalFeatureFreqDistr: DenseVector[Double] = _
    var estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

    def update(): Substep[ProcessingResult] = {
      {
        if (superstep == 0) {
          value.empiricalFeatureFreqDistr = SparseVector.zeros[Double](model.parameters.size)
          val classOffset = model.classOffset(cls)

          encoding.foreachNonZeroPair { (i, v) =>
            val index = classOffset + i
            value.empiricalFeatureFreqDistr(index) = v
          }

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
        estimatedFeatureFreqDistr(0 to estimatedFeatureFreqDistr.size - 1) := 0.0

        val dist = classifier.probClassify(encoding)

        for ((distcls, prob) <- dist) {
          val classOffset = model.classOffset(distcls)

          encoding.foreachNonZeroPair { (i, v) =>
            val index = classOffset + i
            estimatedFeatureFreqDistr(index) += v * Math.exp(prob)
          }
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
        classifier.model.parameters(0 to classifier.model.parameters.size - 1) := incoming.first.value.parameters
        List()
      }
    }
  }

  case class ProcessingResult {
    var empiricalFeatureFreqDistr: SparseVector[Double] = _
    var estimatedFeatureFreqDistr: DenseVector[Double] = _
    var logEmpiricalFeatureFreqDistr: DenseVector[Double] = _
    var parameters: Vector[Double] = _
  }
}