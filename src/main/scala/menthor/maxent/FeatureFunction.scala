package menthor.maxent

abstract class FeatureFunction[C, S] {
	def apply(cls: C, sample: S) : Double
}