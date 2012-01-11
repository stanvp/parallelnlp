package menthor.classifier
package maxent

import scala.collection.mutable.HashMap
import processing.parallel.Graph
import processing.parallel.Message
import processing.parallel.Substep
import processing.parallel.Vertex
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
    val samplesSize = samples.size

    val masters = (for (i <- 0 to partitions - 1) yield new MasterVertex[C, S]("master" + i, classifier, i, null, samplesSize)).toList

    for (master <- masters) {
      master.masters = masters.filterNot(_.label == master.label)
      graph.addVertex(master)
    }

    for ((group, i) <- samples.grouped(Math.ceil(samples.size.toDouble / partitions).toInt).zipWithIndex) {
      for ((c, sample) <- group) {
        val vertex = new SampleVertex(sample.toString, c, sample, i, masters(i))
        graph.addVertex(vertex)
        vertex.connectTo(masters(i))
        masters(i).connectTo(vertex)
      }
    }

    log("Processing samples")

    graph.start()
    graph.iterate(iterations * 5)

    graph.terminate()

    log("Finished MaxentTrainerParallel")

    classifier
  }

  class MasterVertex[C, S <: Sample](label: String, val classifier: MaxentClassifier[C, S], group: Int, var masters: List[MasterVertex[C, S]], val totalSamples: Int)
    extends Vertex[ProcessingResult](label, new ProcessingResult) {

    var iteration = 0

    val model = classifier.model

    var logEmpiricalFeatureFreqDistr: DenseVector[Double] = _

    var lastLoglikelihood = Double.MaxValue
    var loglikelihood = 0.0

    def update(superstep: Int, incoming: List[Message[ProcessingResult]]): Substep[ProcessingResult] = {
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
        }
        List()
      } then {
        // sample step
        List()
      } then {
        val estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

        value.likelihoodsum = 0.0

        incoming.foreach { m =>
          m.value.estimatedFeatureFreqDistr.foreachPair { (i, v) =>
            estimatedFeatureFreqDistr(i) += v
          }

          value.likelihoodsum += m.value.likelihoodsum
        }

        val logEstimatedFeatureFreqDistr = estimatedFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))

        classifier.model.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)

        value.parameters = classifier.model.parameters

        for (master <- masters) yield Message(this, master, this.value)
      } then {
        iteration += 1

        var likelihoodsum = incoming.map(_.value.likelihoodsum).reduce { (x, y) => x + y }
        loglikelihood = Math.log(likelihoodsum / totalSamples)
        
//        if (loglikelihood.isNaN || loglikelihood.isInfinity || loglikelihood > lastLoglikelihood) {
//        	log(label + ": Cutoff at iteration " + (iteration - 1))    
//        	graph.workers.foreach(w => w ! "Stop")
//        } else {
          value.parameters = incoming.map(_.value.parameters).reduce { (x, y) => x + y } / 2

          classifier.model.parameters(0 to classifier.model.parameters.size - 1) := value.parameters

          log(label + ": Iteration " + iteration + " - loglikelihood: " + loglikelihood)
//        }

        lastLoglikelihood = loglikelihood

        List()
      }
    }
  }

  class SampleVertex[C, S <: Sample](label: String, cls: C, sample: S, group: Int, var master: MasterVertex[C, S])
    extends Vertex[ProcessingResult](label, new ProcessingResult) {

    val model = master.classifier.model
    val encoding = model.encode(sample)

    var logEmpiricalFeatureFreqDistr: SparseVector[Double] = _
    var estimatedFeatureFreqDistr = SparseVector.zeros[Double](model.parameters.size)

    def update(superstep: Int, incoming: List[Message[ProcessingResult]]): Substep[ProcessingResult] = {
      {
        if (superstep == 0) {
          value.empiricalFeatureFreqDistr = SparseVector.zeros[Double](model.parameters.size)
          val classOffset = model.classOffset(cls)

          encoding.foreachNonZeroPair { (i, v) =>
            val index = classOffset + i
            value.empiricalFeatureFreqDistr(index) = v
          }

          List(Message(this, master, this.value))
        } else {
          List()
        }
      } then {
        // master step
        List()
      } then {
        estimatedFeatureFreqDistr(0 to estimatedFeatureFreqDistr.size - 1) := 0.0

        val dist = master.classifier.probClassify(encoding)

        for ((distcls, prob) <- dist) {
          val classOffset = model.classOffset(distcls)

          if (distcls == cls) {
            value.likelihoodsum += Math.exp(prob)
          }

          encoding.foreachNonZeroPair { (i, v) =>
            val index = classOffset + i
            estimatedFeatureFreqDistr(index) += v * Math.exp(prob)
          }
        }

        value.estimatedFeatureFreqDistr = estimatedFeatureFreqDistr

        List(Message(this, master, this.value))
      } then {
        // master step
        List()
      } then {
        // master step
        List()
      }
    }
  }

  case class ProcessingResult {
    var empiricalFeatureFreqDistr: Vector[Double] = _
    var estimatedFeatureFreqDistr: Vector[Double] = _
    var parameters: Vector[Double] = _
    var likelihoodsum: Double = 0.0
  }
}