package menthor.documentclassifier

import menthor.util.CollectionUtils._
import menthor.maxent.FeatureGenerator

class DocumentFeatureGenerator(categories : List[Category], features: List[DocumentFeatureFunction], C : Double) 
		extends FeatureGenerator[Category, Document](categories, features, C) {
}