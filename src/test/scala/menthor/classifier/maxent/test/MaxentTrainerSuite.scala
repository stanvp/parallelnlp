package menthor.classifier.maxent.test

import org.scalatest.FunSuite
import menthor.classifier._
import menthor.util._
import menthor.classifier.maxent._
import menthor.apps.Document
import scalala.tensor.dense.DenseVector
import menthor.classifier.featureselector.IGFeatureSelector

class MaxentTrainerSuite extends FunSuite {
  val classes = List("spam", "notspam")

  val samples = List(
    ("spam", new Document("doc1", List("spam"), Map("a" -> 2, "b" -> 1))),
    ("notspam", new Document("doc2", List("notspam"), Map("c" -> 3))),
    ("notspam", new Document("doc3", List("notspam"), Map("d" -> 2, "a" -> 1))),
    ("spam", new Document("doc4", List("spam"), Map("b" -> 1)))
  )
  
  val trainer = new MaxentTrainer[String, Document](new IGFeatureSelector[String](100))

  val features = trainer.selectFeatures(classes, samples)

  val model = new MaxentModel(
    classes,
    features,
    DenseVector.zeros[Double](features.size))

  test("selectFeatures") {
    assert(features.toString() === "List(MaxentFeatureFunction(b, spam), MaxentFeatureFunction(d, spam), MaxentFeatureFunction(c, spam), MaxentFeatureFunction(a, spam), MaxentFeatureFunction(b, notspam), MaxentFeatureFunction(d, notspam), MaxentFeatureFunction(c, notspam), MaxentFeatureFunction(a, notspam))")
  }
  
  test("calculateEmpiricalDistribution") {
    assert(trainer.calculateFeatureFrequencyDistribution(classes, samples, model).toList === List(2.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0))
  }
} 