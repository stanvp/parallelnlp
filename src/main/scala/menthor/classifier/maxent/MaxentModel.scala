package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector
import scala.collection.immutable.HashMap
import scalala.tensor.dense.DenseVector

/**
 * Maximum entropy represents the data needed for classification, and provides sample encoding. 
 * 
 * @param classes list of classification classes
 * @param features list of features that are used to represent samples
 * @param parameters exponential model parameter vector estimated from training data
 *  
 * @author Stanislav Peshterliev 
 */
class MaxentModel[C, S <: Sample](
  val classes: List[C],
  val features: List[Feature],
  val parameters: Vector[Double]) {

  val featuresSize = features.size
  val classOffset = HashMap(classes.zipWithIndex.map(x => (x._1, x._2 * featuresSize)): _*)

  def encode(sample: S): SparseVector[Double] = {
    val encoding = SparseVector.zeros[Double](features.size)

    for ((feature, i) <- features.zipWithIndex) {
      val v = sample.features.get(feature) / sample.total
      if (!v.isNaN()) {
        encoding(i) = v
      }
    }

    encoding
  }

  def parameter(cls: C, i: Int): Double = {
    parameters(classOffset(cls) + i)
  }
}
