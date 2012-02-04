package menthor.apps

import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.Classifier
import menthor.classifier.naivebayes._
import menthor.classifier.maxent._
import scala.collection.mutable.ListBuffer
import scala.util.logging.ConsoleLogger
import menthor.classifier.featureselector.IGFeatureSelector
import gnu.trove.map.hash.TIntDoubleHashMap
import benchmark.TicToc

/**
 * Command line interface for experimenting with 20 Newsgroups corpus 
 * 
 * @author Stanislav Peshterliev
 */
object NewsgroupsClassifier extends TicToc {

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

  def load(folder: String, extendIndex: Boolean): Iterable[Document] = {
    val collection = new ListBuffer[Document]

    forEachFileIn(new File(folder)) {
      file =>
        val source = Source.fromFile(file, "Cp1252")

        val document = new Document(
          file.getName(),
          List(file.getParentFile().getName()),
          Analyzer.termFrequency(source.getLines().mkString("\n"), extendIndex))

        source.close()

        collection += document
    }

    collection.toIterable
  }

  def main(args: Array[String]) {
    if (args.size < 8) {
      println("Please specify [algorithm] [traning mode] [news group corpus train path] [news group corpus test path] [features file] [evaluation] [benchmark result file] [benchmark iteration]")
      println("algorithm can be: maxent or naivebayes")
      println("traning mode can be: parallel, parallelbatch or sequential")
      println("evaluation be: true or false")
      exit
    }

    val algorithm = args.first
    val traningMode = args(1)
    val trainCorpus = args(2)
    val testCorpus = args(3)
    val featuresFile = args(4)
    val evaluation = args(5).toBoolean
    val benchmarkResultFile = args(6)
    val benchmarkIterations = args(7).toInt

    val features = loadFeatures(featuresFile)

    val train = loadCorpus(trainCorpus)

    val samples = train.flatMap(d =>
      d.categories.map(c => (c, d)))

    val trainer = algorithm match {
      case "maxent" =>
        traningMode match {
          case "parallel" => new MaxentTrainerParallel[Category, Document](8, features, 30) with ConsoleLogger
          case "parallelbatch" => new MaxentTrainerParallelBatch[Category, Document](8, features, 30) with ConsoleLogger
          case "sequential" => new MaxentTrainer[Category, Document](features) with ConsoleLogger
          case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
        }
      case "naivebayes" =>
        traningMode match {
          case "parallel" => new NaiveBayesTrainerParallel[Category, Document](8, features) with ConsoleLogger
          case "parallelbatch" => new NaiveBayesTrainerParallelBatch[Category, Document](8, features) with ConsoleLogger
          case "sequential" => new NaiveBayesTrainer[Category, Document](features) with ConsoleLogger
          case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
        }
      case _ => throw new IllegalArgumentException("Illegal algorithm, choose maxent or naivebayes")
    }

    var classifier: Classifier[Category, Document] = null

    for (i <- 1 to benchmarkIterations + 1) {
      if (i == 1) {
        classifier = trainer.train(categories, samples)
      } else {
        tic()

        classifier = trainer.train(categories, samples)

        toc("i_" + (i - 1))
      }
    }

    if (benchmarkIterations > 0) {
      writeTimesLog(benchmarkResultFile)
    }

    if (evaluation == true) {

      println("Evaluation")

      val test = load(testCorpus, false)

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