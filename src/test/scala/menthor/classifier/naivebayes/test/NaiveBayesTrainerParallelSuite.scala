package menthor.classifier.naivebayes.test

import org.scalatest.FunSuite
import menthor.classifier._
import menthor.util._
import menthor.classifier.naivebayes._
import menthor.apps.Document
import menthor.classifier.featureselector.IGFeatureSelector

class NaiveBayesTrainerParallelSuite extends FunSuite {
//  val trainer = new NaiveBayesTrainerParallel(3, new IGFeatureSelector[String](100))
//  
//  import trainer._
//  
//  val classes = List("spam", "notspam")
//  val samples = List(
//    ("spam", new Document("doc1", List("spam"), Map("a" -> 2, "b" -> 1))),
//    ("notspam", new Document("doc2", List("notspam"), Map("c" -> 3))),
//    ("notspam", new Document("doc3", List("notspam"), Map("d" -> 2, "a" -> 1)))
//  )
//
//  test("merge") {
//    val value = new ProcessingResult[String]
//
//    val pr1 = new ProcessingResult[String]
//       
//    pr1.classFeatureBinaryFreqDistr("spam").increment("a", 2)
//    pr1.classFeatureBinaryFreqDistr("spam").increment("b", 1)
//    pr1.classFeatureBinaryFreqDistr("notspam").increment("a", 3)
//    pr1.classFeatureBinaryFreqDistr("notspam").increment("c", 4)
//   
//    pr1.featureBinaryFreqDistr.increment("a", 5)
//    pr1.featureBinaryFreqDistr.increment("b", 1)
//    pr1.featureBinaryFreqDistr.increment("c", 4)
//
//    pr1.classFeatureFreqDistr("spam").increment("a", 2)
//    pr1.classFeatureFreqDistr("spam").increment("b", 1)
//    pr1.classFeatureFreqDistr("notspam").increment("a", 3)
//    pr1.classFeatureFreqDistr("notspam").increment("c", 4)
//   
//    pr1.featureFreqDistr.increment("a", 5)
//    pr1.featureFreqDistr.increment("b", 1)
//    pr1.featureFreqDistr.increment("c", 4)
//    
//    pr1.classSamplesFreqDistr.increment("spam",1)    
//    pr1.classSamplesFreqDistr.increment("notspam",2)
//    
//    val pr2 = new ProcessingResult[String]
//    
//    pr2.classFeatureBinaryFreqDistr("spam").increment("a", 2)
//    pr2.classFeatureBinaryFreqDistr("spam").increment("b", 1)
//    pr2.classFeatureBinaryFreqDistr("notspam").increment("a", 3)
//    pr2.classFeatureBinaryFreqDistr("notspam").increment("c", 4)
//   
//    pr2.featureBinaryFreqDistr.increment("a", 5)
//    pr2.featureBinaryFreqDistr.increment("b", 1)
//    pr2.featureBinaryFreqDistr.increment("c", 4)
//
//    pr2.classFeatureFreqDistr("spam").increment("a", 2)
//    pr2.classFeatureFreqDistr("spam").increment("b", 1)
//    pr2.classFeatureFreqDistr("notspam").increment("a", 3)
//    pr2.classFeatureFreqDistr("notspam").increment("c", 4)
//   
//    pr2.featureFreqDistr.increment("a", 5)
//    pr2.featureFreqDistr.increment("b", 1)
//    pr2.featureFreqDistr.increment("c", 4)
//    
//    pr2.classSamplesFreqDistr.increment("spam",1)    
//    pr2.classSamplesFreqDistr.increment("notspam",2)    
//    
//    ProcessingResult.merge(value, List(pr1, pr2))
    
//    println(value)
//  }
} 