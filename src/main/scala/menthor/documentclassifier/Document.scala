package menthor.documentclassifier

case class Document(name: String, category: String, termFrequency: Map[String, Int]) {
  val totalTerms = termFrequency.values.sum
  override def toString()  = "Document(%s,%s)".format(name, category)
}