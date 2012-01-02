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

object NewsgroupsClassifier {
  def load(folder: String): List[Document] = {
    val collection = new ListBuffer[Document]

    forEachFileIn(new File(folder)) {
      file =>
        val document = new Document(
          file.getName(),
          List(file.getParentFile().getName()),
          Analyzer.termFrequency(Source.fromFile(file, "Cp1252").getLines().mkString("\n")))

        collection += document
    }

    collection.toList
  }

  def main(args: Array[String]) {
    if (args.size < 3) {
      println("Please specify [algorithm] [traning mode] [news group review corpus path] [evaluation]")
      println("algorithm can be: maxent or naivebayes")
      println("traning mode can be: parallel or sequential")
      println("evaluation be: true or false, default is false")
      exit
    }
    
    val evaluation = if (args.length < 4) false else args(3).toBoolean

    val train = load(args(2) + "/20news-bydate-train")    

    val categories = List(
      "alt.atheism",
      "comp.graphics",
      "comp.os.ms-windows.misc",
      "comp.sys.ibm.pc.hardware",
      "comp.sys.mac.hardware",
      "comp.windows.x",
      "misc.forsale",
      "rec.autos",
      "rec.motorcycles",
      "rec.sport.baseball",
      "rec.sport.hockey",
      "sci.crypt",
      "sci.electronics",
      "sci.med",
      "sci.space",
      "soc.religion.christian",
      "talk.politics.guns",
      "talk.politics.mideast",
      "talk.politics.misc",
      "talk.religion.misc")

    val samples = train.flatMap(d =>
      d.categories.map(c => (c, d)))

    val trainer = args.first match {
      case "maxent" =>
        args(1) match {
          case "parallel" => new MaxentTrainerParallel[Category, Document](5, new IGFeatureSelector[Category](6000)) with ConsoleLogger
          case "sequential" => new MaxentTrainer[Category, Document](new IGFeatureSelector[Category](6000)) with ConsoleLogger
          case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
        }
      case "naivebayes" =>
        args(1) match {
          case "parallel" => new NaiveBayesTrainerParallel[Category, Document](10, new IGFeatureSelector[Category](6000)) with ConsoleLogger
          case "sequential" => new NaiveBayesTrainer[Category, Document](new IGFeatureSelector[Category](6000)) with ConsoleLogger
          case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
        }
      case _ => throw new IllegalArgumentException("Illegal algorithm, choose maxent or naivebayes")
    }

    val classifier = trainer.train(categories, samples)

    if (evaluation == true) {
      println("Evaluation")

      val test = load(args(2) + "/20news-bydate-test")

      var success = 0
      for (d <- test) {
        val r = classifier.classify(d)

        if (d.categories.contains(r._1)) {
          success += 1
        }
      }

      println("Total: " + test.size)
      println("Success: " + success)
      println("Percent: " + ((success / test.size.toFloat) * 100) + " %")
    }

    println("Done.")
  }
}