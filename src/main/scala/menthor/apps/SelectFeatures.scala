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

object SelectFeatures {

  def main(args: Array[String]) {
    if (args.size < 4) {
      println("Please specify [corpus] [features size] [corpus path] [output file path]")      
      println("corpus can be: moviereviews, newsgroups or wikipedia")
      exit
    }
    
    val size = args(1).toInt
    val corpusPath = args(2)    
    val outputFilePath = args(3)

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
    
    val fw = new FileWriter(outputFilePath, true)
    
    for ((feature, score) <- features) {
      println(index.get(feature) + " " + score)
      fw.write(index.get(feature) + " " + score + "\n")
    }
      
    fw.close()
  }
}