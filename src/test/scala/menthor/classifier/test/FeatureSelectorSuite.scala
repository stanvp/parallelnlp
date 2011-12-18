package menthor.classifier.test

import org.scalatest.FunSuite

import menthor.classifier._
import menthor.util._

class FeatureSelectorSuite extends FunSuite {
  val classSamplesFreqDistr = new FrequencyDistribution[String]
  classSamplesFreqDistr.increment("spam", 3)
  classSamplesFreqDistr.increment("notspam", 7)
  
  val classFeatureBinaryFreqDistr  = new ConditionalFrequencyDistribution[String, String]
  classFeatureBinaryFreqDistr("spam").increment("cheap", 3)
  classFeatureBinaryFreqDistr("spam").increment("buy", 1)
  classFeatureBinaryFreqDistr("spam").increment("banking", 2)
  classFeatureBinaryFreqDistr("spam").increment("dinner", 0)
  classFeatureBinaryFreqDistr("spam").increment("the", 3)  
  
  classFeatureBinaryFreqDistr("notspam").increment("cheap", 1)
  classFeatureBinaryFreqDistr("notspam").increment("buy", 2)
  classFeatureBinaryFreqDistr("notspam").increment("banking", 2)
  classFeatureBinaryFreqDistr("notspam").increment("dinner", 1)
  classFeatureBinaryFreqDistr("notspam").increment("the", 7)   
  
  val featureBinaryFreqDistr = new FrequencyDistribution[String]
  featureBinaryFreqDistr.increment("cheap", 4)
  featureBinaryFreqDistr.increment("buy", 3)
  featureBinaryFreqDistr.increment("banking", 4)
  featureBinaryFreqDistr.increment("dinner", 1)
  featureBinaryFreqDistr.increment("the", 10)

  val fs = new FeatureSelector[String]

  test("select") {
    val result = fs.select(
      1,
      List("spam", "notspam"),
      List("cheap"),
      classSamplesFreqDistr, 
      classFeatureBinaryFreqDistr,
      featureBinaryFreqDistr
    )
    
    assert(result === List(("cheap", 0.2863530522712403)))
  }
  
  test("test") {
    val result = fs.select(
      2,
      List("spam", "notspam"),
      List("cheap", "buy"),
      classSamplesFreqDistr, 
      classFeatureBinaryFreqDistr,
      featureBinaryFreqDistr
    )
    println(result)
  }  
  
} 