package menthor.classifier.naivebayes.test

import org.scalatest.FunSuite
import menthor.classifier._
import menthor.util._
import menthor.classifier.naivebayes._
import menthor.apps.Document

class NaiveBayesTrainerSuite extends FunSuite {
  
  val classifier = NaiveBayesTrainer.train(
      List("spam", "notspam"),
      List(
    		  ("spam", new Document("doc1", "spam", Map("a" -> 2, "b" -> 1))),
    		  ("notspam", new Document("doc2", "notspam", Map("c" -> 3))),
    		  ("notspam", new Document("doc3", "notspam", Map("d" -> 2, "a" -> 1)))
      )
  )
  
  test("train") {
    assert(classifier.model.classes === List("spam", "notspam"))
    assert(classifier.model.classSamplesFreqDistr.toMap === Map("spam" -> 1.0, "notspam" -> 2.0))
    assert(classifier.model.classFeatureFreqDistr.toMap === Map("spam" -> Map("a" -> 2.0, "b" -> 1.0), "notspam" -> Map("c" -> 3.0, "d" -> 2.0, "a" -> 1)))
    assert(classifier.model.featureFreqDistr.toMap === Map("a" -> 3.0, "b" -> 1.0, "c" -> 3.0, "d" -> 2.0))
  }  

} 