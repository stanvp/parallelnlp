package menthor.classifier

abstract class FeatureFunction[C, S] {
	def apply(cls: C, sample: S) : Double
}