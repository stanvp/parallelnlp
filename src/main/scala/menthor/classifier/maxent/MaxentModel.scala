package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector
import scala.collection.immutable.HashMap
import scalala.tensor.dense.DenseVector

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