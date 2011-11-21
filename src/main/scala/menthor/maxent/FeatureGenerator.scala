package menthor.maxent

import scalala.tensor.dense.DenseVector
import scalala.tensor.mutable.Vector;

abstract class FeatureGenerator[C, S](private val _classes: List[C], private val fs: List[FeatureFunction[C, S]], private val _C: Double) {

  val _features = fs :+ new FeatureFunction[C, S] {
    override def apply(cls: C, sample: S) : Double = {
      C - fs.map(f => f(cls, sample)).sum
    }
  }
  
  def classes = _classes
  def features = _features
  def C = _C

  def generate(cls: C, sample: S): Vector[Double] = DenseVector.tabulate[Double](features.size)(i => features(i)(cls, sample))
  def size: Int = features.size
}