package menthor.documentclassifier

import menthor.maxent.FeatureFunction

class DocumentFeatureFunction(word: String, category: Category) extends FeatureFunction[Category, Document] {
  override def apply(docCategory: Category, doc: Document) : Double = {
    val termFrequency = doc.termFrequency.getOrElse(word, 0)
    if (category != docCategory || termFrequency == 0) {
      0.0
    } else {
      termFrequency
    }
  }
}