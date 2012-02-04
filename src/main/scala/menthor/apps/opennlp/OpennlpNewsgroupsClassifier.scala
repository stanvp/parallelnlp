package menthor.apps.opennlp

import opennlp.tools.cmdline.doccat.DoccatModelLoader
import opennlp.tools.doccat.DocumentCategorizerME
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
import menthor.apps.Document
import menthor.apps.Analyzer

/**
 * Evaluates OpenNLP Maximum Entropy based document categorizer on 20 Newsgroups corpus 
 * 
 * @author Stanislav Peshterliev
 */
object OpennlpNewsgroupsClassifier {

  def load(folder: String): List[(String, String, String)] = {
    val collection = new ListBuffer[(String, String, String)]

    forEachFileIn(new File(folder)) {
      file =>
        val document = (
          file.getName(),
          file.getParentFile().getName(),
          Source.fromFile(file, "Cp1252").getLines().mkString("\n")
       )

        collection += document
    }

    collection.toList
  }

  def main(args: Array[String]) {
    if (args.size < 2) {
      println("Please specify [categorizer model path] [news group corpus path]")
      exit
    }    
    
    println("Opennlp Evaluation")

    val model = new DoccatModelLoader().load(new File(args(0)));
    val doccat = new DocumentCategorizerME(model);  

    val test = load(args(1) + "/20news-bydate-test")

    var success = 0
    for (d <- test) {
	  val prob = doccat.categorize(d._3)
	  val category = doccat.getBestCategory(prob)

      if (d._2 == category) {
        success += 1
      }
    }

    println("Total: " + test.size)
    println("Success: " + success)
    println("Percent: " + ((success / test.size.toFloat) * 100) + " %")

    println("Done.")
  }
}