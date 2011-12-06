package menthor.apps.test

import org.scalatest.FunSuite

import menthor.apps.Document
import menthor.apps.Analyzer

class AnalyzerSuite extends FunSuite {

  val doc = new Document("test1", "label1", Analyzer.termFrequency("aa bb\n aa "));
  
  test("termFrequency") {
	  assert(doc.termFrequency === Map("aa" -> 2, "bb" -> 1)) 
  }
  
  test("termCount") {    
	  assert(doc.termFrequency.size === 2) 
  }  
  
  val doc1 = new Document("test1", "label1", Analyzer.termFrequency("aa bb aa "))
  val doc2 = new Document("test2", "label2", Analyzer.termFrequency("cc dd dd aa bb"))
} 