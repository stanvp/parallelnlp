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
import scalala.library.Numerics._
import java.lang.Math
import menthor.util.ConditionalFrequencyDistribution
import menthor.util.FrequencyDistribution
import scala.util.logging.Logged
import scalala.tensor.sparse.SparseVector
import gnu.trove.procedure.TIntDoubleProcedure
import menthor.util.ProbabilityDistribution
import scala.collection.mutable.ListBuffer

class MaxentTrainer[C, S <: Sample](features: List[Feature], iterations: Int = 100) extends Trainer[C, S] with Logged {
  /**
   * Train maximum entropy classifier
   */
  override def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S] = {

    log("Started MaxentTrainer")

    val model = new MaxentModel[C, S](
      classes,
      features,
      DenseVector.ones[Double](features.size * classes.size))

    val classifier = new MaxentClassifier[C, S](model)
    
    log("Processing samples")
    
    val encodings = Array.ofDim[(C, SparseVector[Double])](samples.size)
    val empiricalFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)
    
    var j = 0
    
    for ((cls, sample) <- samples) {
      val classOffset = model.classOffset(cls)
      val encoding = model.encode(sample)
      
      encodings(j) = ((cls,encoding))
      
      encoding.foreachNonZeroPair { (i, v) =>
        val index = classOffset + i
        empiricalFeatureFreqDistr(index) += v
      }
      
      j += 1
    }
    
    val logEmpiricalFeatureFreqDistr = empiricalFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))

    val estimatedFeatureFreqDistr = DenseVector.zeros[Double](model.parameters.size)

    for (n <- 1 to iterations) {
      log("Iteration: " + n)

      estimatedFeatureFreqDistr(0 to estimatedFeatureFreqDistr.size - 1) := 0.0

      for ((_, encoding) <- encodings) {        
        val dist = classifier.probClassify(encoding)
        
        for ((distcls, prob) <- dist) {
          val classOffset = model.classOffset(distcls)
          
          encoding.foreachNonZeroPair { (i, v) =>
            val index = classOffset + i
            estimatedFeatureFreqDistr(index) += v * Math.exp(prob)
          }
        }
      }
      
      val logEstimatedFeatureFreqDistr = estimatedFeatureFreqDistr.map(x => if (x == 0.0) 0.0 else Math.log(x))
      
      classifier.model.parameters += (logEmpiricalFeatureFreqDistr - logEstimatedFeatureFreqDistr)
    }

    log("Finished MaxentTrainer")

    classifier
  }
}