package menthor.classifier
import gnu.trove.map.hash.TIntDoubleHashMap

/**
 * Sample is represented as feature name to feature value map
 */
trait Sample {
  val features: TIntDoubleHashMap
  val total: Double
}