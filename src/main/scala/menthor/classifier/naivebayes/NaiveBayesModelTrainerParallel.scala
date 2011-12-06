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

object NaiveBayesTrainerParallel {
  def train[C, S <: Sample](
    classes: List[C],
    samples: List[(C, S)],
    partitions: Int,
    featureSelector: FeatureSelector[C] = new FeatureSelector[C]): NaiveBayesClassifier[C, S] = {
    
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

    graph.start()
    graph.iterate(2)

    graph.terminate()

    val value = new ProcessingResult[C]
    merge(value, masters.map(_.value))
    
    val features = featureSelector.select(
      classes,
      value.featureFreqDistr.samples,
      value.classSamplesFreqDistr,
      value.classFeatureBinaryFreqDistr,
      value.featureBinaryFreqDistr).slice(0, 100)

    val model = new NaiveBayesModel[C, S](
      classes,
      features.map(_._1).toList,
      value.classFeatureFreqDistr,
      value.featureFreqDistr,
      value.classSamplesFreqDistr)

    val classifier = new NaiveBayesClassifier[C, S](model)

    classifier
  }
  
  def merge[C](v: ProcessingResult[C], results: List[ProcessingResult[C]]) {
    for (r <- results) {
      for (cls <- r.classSamplesFreqDistr.samples) {
        for (feature <- r.featureFreqDistr.samples) {
          v.classFeatureFreqDistr(cls).increment(feature, r.classFeatureFreqDistr(cls).count(feature))
          
          v.featureFreqDistr.increment(feature, r.featureFreqDistr.count(feature))

          v.classFeatureBinaryFreqDistr(cls).increment(feature, r.classFeatureBinaryFreqDistr(cls).count(feature))
          v.featureBinaryFreqDistr.increment(feature, r.featureBinaryFreqDistr.count(feature))
        }

        v.classSamplesFreqDistr.increment(cls, r.classSamplesFreqDistr.count(cls))
      }
    }
  }  
  
}

class MasterVertex[C, S <: Sample](label: String)
  extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

  def update(): Substep[ProcessingResult[C]] = {
    {
      // sample step
      List()
    } then {
      NaiveBayesTrainerParallel.merge(this.value, incoming.map(_.value))
      List()
    }
  }
}

class SampleVertex[C, S <: Sample](label: String, cls: C, sample: S)
  extends Vertex[ProcessingResult[C]](label, new ProcessingResult[C]) {

  def update(): Substep[ProcessingResult[C]] = {
    {
      for ((feature, v) <- sample.features) {
        value.classFeatureFreqDistr(cls).increment(feature, v)
        value.featureFreqDistr.increment(feature, v)

        value.classFeatureBinaryFreqDistr(cls).increment(feature)
        value.featureBinaryFreqDistr.increment(feature)
      }

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
    var classFeatureBinaryFreqDistr = new ConditionalFrequencyDistribution[C, Feature]
    var featureBinaryFreqDistr = new FrequencyDistribution[Feature]  
}