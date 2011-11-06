package menthor.maxent

import menthor.processing.Vertex
import menthor.processing.Substep
import menthor.processing.Message
import menthor.processing.Graph
import scala.Math
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer


case class Maxent[C, S](classes: List[C], ws: List[Double], featureGenerator: FeatureGenerator[S, C]) {
  var weights = ws

  def classify(sample: S) = probClassify(sample).maxBy(_._2)

  def probClassify(sample: S): Map[C, Double] = {
    val prob = new HashMap[C, Double]
    for (c <- classes) {
      val featureset = featureGenerator.generate(sample, c)
      var total = 0.0
      for ((value, id) <- featureset.zipWithIndex) {
        total += weights(id) * value
      }
      prob.put(c, total)
    }
    normalize(prob.toMap)
  }

  def normalize(dist: Map[C, Double]): Map[C, Double] = {
    val Z = dist.values.map(x => Math.log(x) / Math.log(2)).sum
    dist.mapValues(p => p - Z)
  }
}

object Maxent {
  val iterations = 100
  
  def trainSequential[C, S](classes: List[C], samples: List[(C, S)], featureGenerator: FeatureGenerator[S, C]) = {
    featureGenerator.train(classes, samples)
    val classifier = new Maxent[C, S](classes, featureGenerator.features.map(_ => 1.0), featureGenerator)

    val Cinv = 1.0 / featureGenerator.C //This controls the learning rate: higher Cinv (or lower C) gives faster learning.     

    val empiricalFeatureCounts = featureGenerator.features.map(_ => 0.0).toArray

    for (
      (c, s) <- samples;
      (f, i) <- featureGenerator.features.zipWithIndex
    ) {
      empiricalFeatureCounts(i) += f(s, c)
    }

    val logEmpiricalFeatureCounts = empiricalFeatureCounts.map(x => Math.log(x) / Math.log(2))    

    for (n <- 1 to iterations) {
      println("Iteration: " + n)

      val estimatedFeatureCounts = featureGenerator.features.map(_ => 0.0).toArray

      for ((_, s) <- samples) {
        val dist = classifier.probClassify(s)
        for (
          (c, prob) <- dist;
          (f, i) <- featureGenerator.features.zipWithIndex
        ) {
          estimatedFeatureCounts(i) += prob * f(s, c)
        }
      }

      val logEstimatedFeatureCounts = estimatedFeatureCounts.map(x => Math.log(x) / Math.log(2))

      val diff = logEmpiricalFeatureCounts.zip(logEstimatedFeatureCounts).map { case (x, y) => (x - y) * Cinv }
      classifier.weights = classifier.weights.zip(diff).map { case (x, y) => x + y }

      println("Weights: " + classifier.weights)
    }

    classifier
  }
  
  def trainParallel[C, S](classes: List[C], samples: List[(C, S)], featureGenerator: FeatureGenerator[S, C]) = {
    featureGenerator.train(classes, samples)
    val classifier = new Maxent[C, S](classes, featureGenerator.features.map(_ => 1.0), featureGenerator)
    
    val graph = new Graph[ProcessingResult[C,S]]
    
    for ((c, sample) <- samples) {
      val processingResult = new ProcessingResult(c, sample)
      graph.addVertex(new SampleVertex(sample.toString, classifier, processingResult))
    }
    
    graph.start()
    graph.iterate(iterations + 3)

    graph.terminate()
    
    classifier
  }  
}

class SampleVertex[C, S](label: String, classifier: Maxent[C, S], processingResult: ProcessingResult[C, S]) extends Vertex[ProcessingResult[C, S]](label, processingResult) {
  def classifer = classifier
  def numVertices = graph.vertices.size
 
  def update() : Substep[ProcessingResult[C, S]] = {
    {		
	  processingResult.empiricalFeatureCounts = classifier.featureGenerator.generate(processingResult.sample, processingResult.c)
	  for (neighbor <- graph.vertices) yield Message(this, neighbor, this.value)	  
    } then {
//    	for (message <- incoming) {
//    	  // sum and log empirical counts 
//    	}
      List()
    }
  }
}

case class ProcessingResult[C, S](c: C, sample: S) {
  var empiricalFeatureCounts : List[Double] = _
  var estimatedFeatureCounts : List[Double] = _
}