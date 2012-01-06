package menthor.apps

import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.naivebayes.NaiveBayesTrainer
import menthor.classifier.naivebayes.NaiveBayesTrainerParallel
import menthor.classifier.maxent.MaxentTrainer
import menthor.classifier.maxent.MaxentTrainerParallel
import scala.collection.mutable.ListBuffer
import scala.util.logging.ConsoleLogger
import menthor.classifier.featureselector.IGFeatureSelector
import java.io.FileWriter
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.procedure.TObjectIntProcedure
import gnu.trove.procedure.TIntDoubleProcedure

object BuildCorpus {

  def main(args: Array[String]) {
    if (args.size < 4) {
      println("Please specify [corpus] [features size] [corpus path] [output corpus file path] [output feature file path]")      
      println("corpus can be: moviereviews, newsgroups or wikipedia")
      exit
    }
    
    val size = args(1).toInt
    val corpusPath = args(2)
    val corpusFilePath = args(3)
    val featuresFilePath = args(4)

    val (collection, categories) = args.first match {
      case "moviereviews" => (MovieReviewClassifier.load(corpusPath, true), MovieReviewClassifier.categories)
      case "newsgroups" => (NewsgroupsClassifier.load(corpusPath, true), NewsgroupsClassifier.categories)
      case "wikipedia" => (WikipediaClassifier.load(corpusPath, true), WikipediaClassifier.categories)
      case _ => throw new IllegalArgumentException("Illegal corpus, choose moviereview, newsgroups or wikipedia")
    }
    
    val samples = collection.flatMap(d =>
      d.categories.map(c => (c, d)))
      
    val ig = new IGFeatureSelector[Category, Document](size)
      
    val features = ig.select(categories, samples)
    
    val index = new TIntObjectHashMap[String]()
      
    Analyzer.index.forEachEntry(new TObjectIntProcedure[String] {
      override def execute(key: String, value: Int) : Boolean = {
        index.put(value, key)
        true
      }
    })
    
    val featuresFile = new FileWriter(featuresFilePath, true)
    
    for ((feature, score) <- features) {
      println(feature + " | " + index.get(feature) + " | " + score)
      featuresFile.write(feature + " | " + index.get(feature) + " | " + score + "\n")
    }
      
    featuresFile.close()
    
    val corpusFile = new FileWriter(corpusFilePath, true) 
    
    for (document <- collection) {
      val encoding = ListBuffer[String]()
      document.termFrequency.forEachEntry(new TIntDoubleProcedure {
        override def execute(key: Int, value: Double) : Boolean = {
          encoding += key + ":" + value
          true
        }
      })
      
      corpusFile.write(document.categories.mkString(",") + " | " + document.name + " | " + encoding.mkString(" ") + "\n")
    }
    
    corpusFile.close()
  }
}