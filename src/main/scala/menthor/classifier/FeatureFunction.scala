package menthor.classifier

trait FeatureFunction[C, S <: Sample] {
	def apply(cls: C, sample: S) : Double
}