package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector

class MaxentModel[C, S <: Sample](
  val classes: List[C],
  val features: List[Feature],
  val parameters: Vector[Double]) {

  def encode(cls: C, sample: S): SparseVector[Double] = {
    val encoding = SparseVector.zeros[Double](parameters.size)
    val clsIndex = classes.indexOf(cls) + 1

    for ((feature, i) <- features.zipWithIndex) {
      encoding(i * clsIndex) = sample.features.getOrElse(feature, 0.0) / sample.total
    }

    encoding
  }
}