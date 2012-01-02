package menthor.classifier

/**
 * Sample is represented as feature name to feature value map
 */
trait Sample {
  val features: Map[Feature, Double]
  val total: Double
}