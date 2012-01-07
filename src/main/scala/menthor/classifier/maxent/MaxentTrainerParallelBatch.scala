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

class MaxentTrainerParallelBatch[C, S <: Sample](partitions: Int, features: List[Feature], iterations: Int = 100) extends Trainer[C, S] with Logged {
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

    for ((group, i) <- samples.grouped(Math.ceil(samples.size.toDouble / partitions).toInt).zipWithIndex) {
      val vertex = new MasterVertex[C, S]("master" + i, classifier, group)
      graph.addVertex(vertex)
    }

    log("Processing samples")

    graph.start()
    graph.iterate(iterations * 7)

    graph.terminate()

    log("Finished MaxentTrainerParallel")

    classifier
  }

  class MasterVertex[C, S <: Sample](label: String, val classifier: MaxentClassifier[C, S], samples: Iterable[(C, S)])
    extends Vertex[ProcessingResult](label, new ProcessingResult) {

    var iteration = 0
    val samplesSize = samples.size

    val model = classifier.model
    val encodings = Array.ofDim[(C, SparseVector[Double])](samples.size)

    var logEmpiricalFeatureFreqDistr: DenseVector[Double] = _
    val estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

    var likelihoodsum = 0.0
    var lastLoglikelihood = Double.MaxValue
    var loglikelihood = 0.0
    
    value.parameters = DenseVector.zeros[Double](model.parameters.size)

    def update(): Substep[ProcessingResult] = {
      {
        if (superstep == 0) {
          val empiricalFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

          var j = 0

          for ((cls, sample) <- samples) {
            val classOffset = model.classOffset(cls)
            val encoding = model.encode(sample)

            encodings(j) = (cls, encoding)

            encoding.foreachNonZeroPair { (i, v) =>
              val index = classOffset + i
              empiricalFeatureFreqDistr(index) += v
            }

            j += 1
          }

          logEmpiricalFeatureFreqDistr = empiricalFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))

          for (neighbor <- graph.vertices) yield Message(this, neighbor, this.value)
        } else {
          List()
        }
      } then {
        estimatedFeatureFreqDistr(0 to estimatedFeatureFreqDistr.size - 1) := 0.0

        likelihoodsum = 0.0

        for ((cls, encoding) <- encodings) {
          val dist = classifier.probClassify(encoding)

          for ((distcls, prob) <- dist) {
            val classOffset = model.classOffset(distcls)

            if (distcls == cls) {
              likelihoodsum += Math.exp(prob)
            }

            encoding.foreachNonZeroPair { (i, v) =>
              val index = classOffset + i
              estimatedFeatureFreqDistr(index) += v * Math.exp(prob)
            }
          }
        }

        loglikelihood = Math.log(likelihoodsum / samplesSize)

        if (loglikelihood.isNaN || loglikelihood.isInfinity || loglikelihood > lastLoglikelihood) {
          log("Cutoff at iteration " + (iteration - 1))
          graph.workers.foreach(w => w ! "Stop")
          List()
        } else {
          val logEstimatedFeatureFreqDistr = estimatedFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))
          value.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)

          lastLoglikelihood = loglikelihood
          
          log("Iteration: " + iteration + " - loglikelihood: " + loglikelihood)
          
          for (neighbor <- graph.vertices) yield Message(this, neighbor, this.value)
        }                
      } then {        
    	  value.parameters = (incoming.map(_.value.parameters).reduce { (x, y) => x + y } / graph.vertices.size)
          classifier.model.parameters(0 to classifier.model.parameters.size - 1) := value.parameters
          
          List()
      }
    }
  }

  case class ProcessingResult {
    var parameters: Vector[Double] = _
  }
}