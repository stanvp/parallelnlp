package menthor.apps

import menthor.classifier.Sample

import scalala.operators.Implicits._

case class Document(name: String, category: String, termFrequency: Map[String, Double]) extends Sample {
  val totalTerms = termFrequency.values.sum
  override def toString()  = "Document(%s,%s)".format(name, category)
  
  override def total: Double = totalTerms
  override def features : Map[String, Double] = termFrequency
}