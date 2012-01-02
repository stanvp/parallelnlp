package menthor.classifier

/**
 * Sample is represented as feature name to feature value map
 */
trait Sample {
	def features : Map[Feature, Double]	
	def total: Double
}