package menthor.classifier
package naivebayes

import menthor.processing.Graph
import menthor.processing.Message
import menthor.processing.Substep
import menthor.processing.Vertex
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

class NaiveBayesTrainerParallelBatch[C, S <: Sample](partitions: Int, features: List[Feature]) extends Trainer[C, S] with Logged {
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): NaiveBayesClassifier[C, S] = {

    log("Started NaiveBayesTrainerParallelBatch")

    log("Building the graph")

    val graph = new Graph[ProcessingResult[C]]

    for ((group, i) <- samples.grouped(Math.ceil(samples.size.toDouble / partitions).toInt).zipWithIndex) {
      val master = new MasterVertex[C, S]("master" + i, group)
      graph.addVertex(master)
    }

    log("Processing samples")

    graph.start()
    graph.iterate(2)

    graph.terminate()
    
    val value = graph.vertices.first.value

    log("Aggregating results")

    val model = new NaiveBayesModel[C, S](
      classes,
      features,
      value.classFeatureFreqDistr,
      value.featureFreqDistr,
      value.classSamplesFreqDistr)

    val classifier = new NaiveBayesClassifier[C, S](model)

    log("Finished NaiveBayesTrainerParallelBatch")

    classifier
  }

  

  class MasterVertex[C, S <: Sample](label: String, samples: Iterable[(C, S)])
    extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

    def update(): Substep[ProcessingResult[C]] = {
      {
        for ((cls, sample) <- samples) {
          sample.features.forEachEntry(new TIntDoubleProcedure {
            override def execute(feature: Int, v: Double): Boolean = {
              value.classFeatureFreqDistr(cls).increment(feature, v)
              value.featureFreqDistr.increment(feature, v)

              true
            }
          })

          value.classSamplesFreqDistr.increment(cls)
        }
        for (neighbor <- graph.vertices) yield Message(this, neighbor, this.value)
      } then {
        ProcessingResult.merge(value, incoming.map(_.value))
        List()
      }
    }
  }
}