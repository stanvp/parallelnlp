package menthor.classifier

abstract class FeatureTrainer[C,S] {
	def train(classes: List[C], samples: List[(C,S)]) : FeatureGenerator[C,S]
}