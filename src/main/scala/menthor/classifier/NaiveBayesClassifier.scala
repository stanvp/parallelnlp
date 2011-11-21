package menthor.classifier

import menthor.processing.Graph
import menthor.processing.Message
import menthor.processing.Substep
import menthor.processing.Vertex
import menthor.util.MathUtils._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import scala.collection.mutable.HashMap

class NaiveBayesClassifier[C,S](classes: List[C], stats: Stats[C], featureGenerator: FeatureGenerator[C, S]) extends Classifier[C,S] {
	override def classify(sample: S) : (C,Double) = probClassify(sample).maxBy(_._2)
	
  val mu = 1
	
  def probClassify(sample: S): Map[C, Double] = {
    val prob = new HashMap[C, Double]
    for (cls <- classes) {
      val featureset = featureGenerator.generate(cls, sample)
      
      val prior = log2(stats.sampleClassTotal(cls)) - log2(stats.sampleTotal)
      
      var total = prior

      featureset.foreachPair { (id, value) =>
        val t11 = stats.classFeatureCounts(cls)(id)
        val t22 = stats.featureCounts(id)
        val t33 = stats.featureTotal
        val p1 = log2(stats.classFeatureCounts(cls)(id) + (mu*(stats.featureCounts(id)/stats.featureTotal))) 
        
        val t1 = stats.featureClassTotal(cls)
        val p2 = log2(stats.featureClassTotal(cls) + mu)
        total += value * (p1 - p2)
      }

      prob.put(cls, total)
    }

    normalizeLogProbabilty(prob.toMap)
  }	
}

case class Stats[C](classes: List[C], classFeatureCounts: Map[C, Vector[Double]]) {
  val featureCounts = classFeatureCounts.values.reduce(_ + _)
  
  val featureClassTotal = classFeatureCounts.mapValues(x => x.sum)
  
  val featureTotal = featureClassTotal.values.sum
  
  val sampleClassTotal = classFeatureCounts.mapValues(x => x.size)
  val sampleTotal = sampleClassTotal.values.sum
}

object NaiveBayesClassifier {
  def trainSequential[C, S](classes: List[C], samples: List[(C, S)], featureTrainer: FeatureTrainer[C, S]) = {
    val featureGenerator = featureTrainer.train(classes, samples)
    trainSequential[C, S](classes, samples, featureGenerator)
  }

  def trainSequential[C, S](classes: List[C], samples: List[(C, S)], featureGenerator: FeatureGenerator[C, S]) = {
    val classFeatureCounts = new HashMap[C, Vector[Double]]
    
    for ((cls, sample) <- samples) {
      if (!classFeatureCounts.contains(cls)) {
        classFeatureCounts.put(cls, DenseVector.zeros[Double](featureGenerator.size))
      }
      
      classFeatureCounts(cls) += featureGenerator.generate(cls, sample)
    }
    
    val classifier = new NaiveBayesClassifier[C, S](classes, new Stats[C](classes, classFeatureCounts.toMap), featureGenerator)
    
    classifier
  }
}