package menthor.classifier
package maxent

import scala.collection.mutable.HashMap
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.operators.Implicits._
import scalala.scalar._
import scalala.tensor.dense._
import scalala.tensor.mutable._
import scala.Math
import menthor.util.ProbabilityDistribution
import scalala.tensor.sparse.SparseVector

/**
 * Maximum entropy classifier
 *
 * Uses logarithmic parameter weights
 *
 * For more information see the tutorial available at http://nlp.stanford.edu/software/classifier.shtml
 * Christopher Manning and Dan Klein. 2003. Optimization, Maxent Models, and Conditional Estimation without Magic. Tutorial at HLT-NAACL 2003 and ACL 2003.
 *
 */
case class MaxentClassifier[C, S <: Sample](val model: MaxentModel[C, S]) extends Classifier[C, S] {
  
  override def probClassify(sample: S): ProbabilityDistribution[C] = {
    probClassify(model.encode(sample))
  }  
  
  def probClassify(encoding: SparseVector[Double]): ProbabilityDistribution[C] = {
    val prob = new HashMap[C, Double]

    for (cls <- model.classes) {
      var total = 0.0

      encoding.foreachNonZeroPair { (index, value) =>
        val parameter = model.parameter(cls, index) 
        total += parameter * value
      }

      prob.put(cls, total)
    }

    new ProbabilityDistribution(prob.toMap, false)
  }
}