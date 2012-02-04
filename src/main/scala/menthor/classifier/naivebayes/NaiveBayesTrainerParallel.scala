package menthor.classifier
package naivebayes

import processing.parallel.Graph
import processing.parallel.Message
import processing.parallel.Substep
import processing.parallel.Vertex
import menthor.classifier.FeatureSelector
import menthor.classifier.Sample
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.util.logging.Logged
import gnu.trove.procedure.TIntDoubleProcedure

/**
 * Parallel Naive Bayes classifier trainer that implements "Vertex for every sample" strategy.
 * For more information @see the technical report.
 * 
 * @param partitions number of partitions to split the data
 * @param features set of features to represent samples
 * 
 * @author Stanislav Peshterliev
 */
class NaiveBayesTrainerParallel[C, S <: Sample](partitions: Int, features: List[Feature]) extends Trainer[C, S] with Logged {
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): NaiveBayesClassifier[C, S] = {

    log("Started NaiveBayesTrainerParallel")

    log("Building the graph")

    val graph = new Graph[ProcessingResult[C]]
  
    val masters = (for (i <- 0 to partitions - 1) yield new MasterVertex[C, S]("master" + i)).toList

    for (master <- masters) {
      graph.addVertex(master)
    }

    // partition samples
    for ((group, i) <- samples.grouped(Math.ceil(samples.size.toDouble / partitions).toInt).zipWithIndex) {
      for ((c, sample) <- group) {
        val vertex = new SampleVertex(sample.toString, c, sample)
        graph.addVertex(vertex)
        vertex.connectTo(masters(i))
        masters(i).connectTo(vertex)
      }
    }

    log("Processing samples")

    graph.start()
    graph.iterate(2)

    graph.terminate()

    log("Aggregating results")

    val value = new ProcessingResult[C]
    ProcessingResult.merge(value, masters.map(_.value))

    val model = new NaiveBayesModel[C, S](
      classes,
      features,
      value.classFeatureFreqDistr,
      value.featureFreqDistr,
      value.classSamplesFreqDistr)

    val classifier = new NaiveBayesClassifier[C, S](model)

    log("Finished NaiveBayesTrainerParallel")

    classifier
  }

  /**
   * Master vertex implements aggregate computation
   */
  class MasterVertex[C, S <: Sample](label: String)
    extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

    def update(superstep: Int, incoming: List[Message[ProcessingResult[C]]]): Substep[ProcessingResult[C]] = {
      {
        // sample step
        List()
      } then {
        ProcessingResult.merge(value, incoming.map(_.value))
        List()
      }
    }
  }
   
  /**
   * Sample vertex implements the computation on every sample
   */
  class SampleVertex[C, S <: Sample](label: String, cls: C, sample: S)
    extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

    def update(superstep: Int, incoming: List[Message[ProcessingResult[C]]]): Substep[ProcessingResult[C]] = {
      {
        sample.features.forEachEntry(new TIntDoubleProcedure {
          override def execute(feature: Int, v: Double): Boolean = {
            value.classFeatureFreqDistr(cls).increment(feature, v)
            value.featureFreqDistr.increment(feature, v)

            true
          }
        })

        value.classSamplesFreqDistr.increment(cls)

        for (neighbor <- neighbors) yield Message(this, neighbor, this.value)
      } then {
        // master step
        List()
      }
    }
  }
}