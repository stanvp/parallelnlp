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

  class MasterVertex[C, S <: Sample](label: String)
    extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

    def update(): Substep[ProcessingResult[C]] = {
      {
        // sample step
        List()
      } then {
        ProcessingResult.merge(value, incoming.map(_.value))
        List()
      }
    }
  }

  class SampleVertex[C, S <: Sample](label: String, cls: C, sample: S)
    extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

    def update(): Substep[ProcessingResult[C]] = {
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

  class ProcessingResult[C] {
    var classFeatureFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    var featureFreqDistr = new FrequencyDistribution[Feature]
    var classSamplesFreqDistr = new FrequencyDistribution[C]
  }

  object ProcessingResult {
    def merge[C](v: ProcessingResult[C], results: List[ProcessingResult[C]]) {
      v.classFeatureFreqDistr = ConditionalFrequencyDistribution.merge(results.map(_.classFeatureFreqDistr))
      v.featureFreqDistr = FrequencyDistribution.merge(results.map(_.featureFreqDistr))
      v.classSamplesFreqDistr = FrequencyDistribution.merge(results.map(_.classSamplesFreqDistr))
    }
  }
}