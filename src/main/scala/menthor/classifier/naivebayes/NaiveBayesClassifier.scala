package menthor.classifier
package naivebayes 

import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import scala.collection.mutable.HashMap
import menthor.util.ProbabilityDistribution

class NaiveBayesClassifier[C, S <: Sample](val model: NaiveBayesModel[C, S]) extends Classifier[C,S] {
  val mu = 0.1
	
  override def probClassify(sample: S): ProbabilityDistribution[C] = {
    val prob = new HashMap[C, Double]
    
    for (cls <- model.classes) {
      var total : Double = Math.log(model.classSamplesFreqDistr.frequency(cls))

      for ((name, value) <- model.encode(sample)) {
        val p1 = model.classFeatureFreqDistr(cls).count(name) + (mu*(model.featureFreqDistr.frequency(name)))
        val p2 = model.classFeatureFreqDistr(cls).total + mu
        total += value * (Math.log(p1) - Math.log(p2))
      }

      prob.put(cls, total)
    }

    new ProbabilityDistribution(prob.toMap, true)
  }
}