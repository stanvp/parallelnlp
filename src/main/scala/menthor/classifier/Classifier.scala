package menthor.classifier

abstract class Classifier[C,S] {
	def classify(sample: S) : (C,Double)
}